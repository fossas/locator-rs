#![doc = include_str!("../../README.md")]
#![deny(unsafe_code)]
#![deny(missing_docs)]
#![warn(rust_2018_idioms)]

use std::{borrow::Cow, convert::Infallible, num::ParseIntError, str::FromStr};

use bon::Builder;
use compact_str::{CompactString, ToCompactString, format_compact};
use derivative::Derivative;
use derive_more::{Debug, Display};
use documented::Documented;
use duplicate::duplicate;
use lazy_regex::regex_is_match;
use serde::{Deserialize, Serialize, Serializer, de::IntoDeserializer};
use serde_json::json;
use utoipa::{
    PartialSchema, ToSchema,
    openapi::{ObjectBuilder, Type},
};
use versions::Versioning;

pub mod constraint;
mod error;
mod locator;
mod locator_package;
mod locator_strict;

pub use constraint::*;
pub use ecosystems::{Ecosystem, EcosystemPrivate, EcosystemPublic};
pub use error::*;
pub use locator::*;
pub use locator_package::*;
pub use locator_strict::*;

#[doc(hidden)]
pub use semver;
#[doc(hidden)]
pub use versions;

/// Identifies supported code host ecosystems.
///
/// Packages have names and versions, like `lodash@1.0.0` or `sqlx@0.8.5`.
/// But these names and versions are only fully specified inside of the context of an ecosystem-
/// for example when we say `sqlx`, do we mean `https://github.com/launchbadge/sqlx` or `https://github.com/jmoiron/sqlx`?
/// The ecosystems in this module help us disambiguate this.
///
/// On top of this, unfortunately FOSSA also has "ecosystems" that are private to FOSSA and have no meaning
/// outside of the context of FOSSA; these are categorized according to the below options.
///
/// ## [`Ecosystem`]
///
/// The top level enum of all available ecosystems.
/// This is a superset of all other kinds of ecosystem, like [`EcosystemPublic`] and [`EcosystemPrivate`].
///
/// For example:
/// - `Npm` implies "uses the NPM ecosystem", meaning the code referenced is distributed with the NPM package manager.
/// - `Git` implies "uses the git ecosystem", meaning the code referenced is distributed with a git server.
/// - `Archive` is an indicator for an `archive` project in FOSSA, which is a blob of uploaded source code.
/// - `Custom` is used for top-level projects in FOSSA (not all top-level projects use custom, but custom always means this).
///
/// ## [`EcosystemPublic`]
///
/// Most listed ecosystems are public ecosystems.
///
/// For example:
/// - `Npm` implies "uses the NPM ecosystem", meaning the code referenced is distributed with the NPM package manager.
/// - `Git` implies "uses the git ecosystem", meaning the code referenced is distributed with a git server.
///
/// ## [`EcosystemPrivate`]
///
/// Most ecosystems are "public", meaning they're not FOSSA controlled.
///
/// However, some ecosystems are FOSSA-specific; these mean nothing
/// outside of the context of FOSSA.
///
/// For example:
/// - `Archive` is an indicator for an `archive` project in FOSSA, which is a blob of uploaded source code.
/// - `Custom` is used for top-level projects in FOSSA (not all top-level projects use custom, but custom always means this).
///
/// ## Standalone ecosystem types
///
/// Each ecosystem also has a standalone struct, for example:
/// - [`Go`]
/// - [`Npm`]
/// - [`Archive`]
/// - [`Custom`]
///
/// ## Conversions
///
/// This module implements conversions from types according to the following rules:
/// - If the source type is known to be a strict subset of the destination type, this is implemented as an infallible `From` conversion.
/// - Otherwise, this is implemented as a fallible `TryFrom` conversion, where errors return [`InvalidConversionError`].
#[locator_codegen::ecosystems(
    /// Interacts with Bower.
    Public => Bower, "bower";
    /// Interacts with Carthage.
    Public => Cart, "cart";
    /// Interacts with Cargo.
    Public => Cargo, "cargo";
    /// Interacts with Composer.
    Public => Comp, "comp";
    /// Interacts with Conan.
    Public => Conan, "conan";
    /// Interacts with Conda.
    Public => Conda, "conda";
    /// Interacts with CPAN.
    Public => Cpan, "cpan";
    /// Interacts with CRAN.
    Public => Cran, "cran";
    /// Interacts with RubyGems.
    Public => Gem, "gem";
    /// Interacts with git VCS hosts.
    Public => Git, "git";
    /// Interacts with Go projects.
    Public => Go, "go";
    /// Interacts with Hackage.
    Public => Hackage, "hackage";
    /// Interacts with Hex.
    Public => Hex, "hex";
    /// Interacts with Linux Alpine package managers.
    Public => LinuxAlpine, "apk";
    /// Interacts with Linux Debian package managers.
    Public => LinuxDebian, "deb";
    /// Interacts with Linux RPM package managers.
    Public => LinuxRpm, "rpm-generic";
    /// Interacts with Maven.
    Public => Maven, "mvn";
    /// Interacts with NPM.
    Public => Npm, "npm";
    /// Interacts with Nuget.
    Public => Nuget, "nuget";
    /// Interacts with PyPI.
    Public => Pip, "pip";
    /// Interacts with CocoaPods.
    Public => Pod, "pod";
    /// Interacts with Dart's package manager.
    Public => Pub, "pub";
    /// Interact with Swift's package manager.
    Public => Swift, "swift";
    /// Specifies arbitrary code at an arbitrary URL.
    Public => Url, "url";

    /// Archive locators are FOSSA specific.
    Private => Archive, "archive";
    /// Interacts with projects from CodeSentry
    Private => CodeSentry, "csbinary";
    /// The `custom` ecosystem describes first party projects in FOSSA.
    Private => Custom, "custom";
    /// Indicates a specific RPM file.
    Private => Rpm, "rpm";
    /// An unresolved path dependency.
    Private => UnresolvedPath, "upath";
    /// A user-specified package.
    Private => User, "user";
)]
pub struct ecosystems;

/// Identifies the organization to which this locator is namespaced.
///
/// Organization IDs are canonically created by FOSSA instances
/// and have no meaning outside of FOSSA instances.
#[derive(
    Copy,
    Clone,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Serialize,
    Deserialize,
    Hash,
    Documented,
    ToSchema,
    Display,
    Debug,
)]
#[schema(example = json!(1))]
#[display("{}", self.0)]
pub struct OrgId(usize);

impl From<OrgId> for usize {
    fn from(value: OrgId) -> Self {
        value.0
    }
}

impl From<usize> for OrgId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl FromStr for OrgId {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
    }
}

impl TryFrom<&str> for OrgId {
    type Error = <usize as FromStr>::Err;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(OrgId(value.parse()?))
    }
}

duplicate! {
    [
        number;
        [ u64 ];
        [ u32 ];
        [ u16 ];
        [ u8 ];
        [ i64 ];
        [ i32 ];
        [ i16 ];
        [ i8 ];
        [ isize ];
    ]
    impl From<OrgId> for number {
        fn from(value: OrgId) -> Self {
            value.0 as number
        }
    }
    impl From<number> for OrgId {
        fn from(value: number) -> Self {
            Self(value as usize)
        }
    }
}

/// The package section of the locator.
///
/// A "package" is generally the name of a project or dependency in a code host.
/// However some ecosystem ecosystems (such as `git`) embed additional information
/// inside the `Package` of a locator, such as the URL of the `git` instance
/// from which the project can be fetched.
///
/// Additionally, some ecosystem ecosystems (such as `apk`, `rpm-generic`, and `deb`)
/// further encode additional standardized information in the `Package` of the locator.
#[derive(Clone, Eq, PartialEq, Hash, Display, Debug, Serialize, Deserialize, Documented)]
#[display("{}", self.0)]
pub struct Package(CompactString);

impl Package {
    /// View the item as a string.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl<S: Into<CompactString>> From<S> for Package {
    fn from(value: S) -> Self {
        Self(value.into())
    }
}

impl From<&Package> for Package {
    fn from(value: &Package) -> Self {
        value.clone()
    }
}

impl std::cmp::Ord for Package {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        alphanumeric_sort::compare_str(&self.0, &other.0)
    }
}

impl std::cmp::PartialOrd for Package {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialSchema for Package {
    fn schema() -> utoipa::openapi::RefOr<utoipa::openapi::schema::Schema> {
        ObjectBuilder::new()
            .description(Some(Self::DOCS))
            .examples([json!("github.com/fossas/locator-rs"), json!("lodash")])
            .min_length(Some(1))
            .schema_type(Type::String)
            .build()
            .into()
    }
}

impl ToSchema for Package {
    fn name() -> Cow<'static, str> {
        Cow::Borrowed("Package")
    }
}

/// A parsed version.
///
/// This type tries to do its best to handle arbitrary version schemes:
/// - SemVer, like `1.2.3-r1` (via `Version`)
/// - Structured versions, like `1:2.3.4` (via `Version`)
/// - Grab bags, like `2:10.2+0.0093r3+1-1` (via `Version`)
/// - Calendar versions, like `20250101-1` (via `Opaque`)
/// - Opaque strings, like `abcd` (via `Opaque`)
///
/// As a special case, it also proactively trims leading `v` characters
/// from versions before trying to parse them;
/// this means it can also support strings like `v1.2.3` as though they were `SemVer`.
#[derive(Derivative, Documented, Display, Clone, Debug)]
#[derivative(Eq, PartialEq, Ord, PartialOrd, Hash)]
#[display("{}", self.input)]
pub struct Version {
    /// The parsed version.
    parsed: Versioning,

    /// The original input.
    ///
    /// Stored so that:
    /// - We can cheaply reference it when doing string comparisons
    /// - We can use the input value instead of the parsed value when printing
    ///   (since the parsed value may be different, e.g. if there's a `v` prefix).
    #[derivative(PartialEq = "ignore", Ord = "ignore", Hash = "ignore")]
    input: CompactString,
}

impl Version {
    /// View the original input as a string.
    pub fn as_str(&self) -> &str {
        self.input.as_str()
    }

    /// Create a new SemVer variant version.
    pub fn new_semver(major: u32, minor: u32, patch: u32) -> Self {
        let parsed = Versioning::Ideal(versions::SemVer {
            major,
            minor,
            patch,
            ..Default::default()
        });
        let input = format_compact!("{major}.{minor}.{patch}");
        Self { parsed, input }
    }

    /// Try to parse the input string as a version.
    ///
    /// Accepts version strings with `v` prefixes as a special case.
    pub fn parse(input: impl AsRef<str>) -> Option<Self> {
        let input = input.as_ref();

        // `Versioning` is a little too permissive; it handles more arbitrary strings than we'd prefer.
        // For example, it happily parses the input string 'b' as `Versioning::General(...)`,
        // while we'd rather hand that over to our `Opaque` handling.
        //
        // The intention here is to only pass in strings that _start with_ a digit
        // (optionally preceded by `v`) to `Versioning`.
        let parsed = if regex_is_match!(r"^v?\d+.*", input) {
            Versioning::new(input.trim_start_matches('v'))
        } else {
            None
        }?;

        let input = input.to_compact_string();
        Some(Self { input, parsed })
    }
}

impl PartialSchema for Version {
    fn schema() -> utoipa::openapi::RefOr<utoipa::openapi::schema::Schema> {
        ObjectBuilder::new()
            .description(Some(Self::DOCS))
            .examples([
                json!("1.0.0"),
                json!("v1.0.0"),
                json!("2:10.2+0.0093r3+1-1"),
            ])
            .min_length(Some(1))
            .schema_type(Type::String)
            .build()
            .into()
    }
}

impl ToSchema for Version {
    fn name() -> Cow<'static, str> {
        Cow::Borrowed("Version")
    }
}

/// The revision section of the locator.
///
/// A "revision" is the version of the project in the code host.
/// Some ecosystem ecosystems (such as `apk`, `rpm-generic`, and `deb`)
/// encode additional standardized information in the `Revision` of the locator.
///
/// This type tries to do its best to handle arbitrary version schemes:
/// - SemVer, like `1.2.3-r1` (via `Version`)
/// - Structured versions, like `1:2.3.4` (via `Version`)
/// - Grab bags, like `2:10.2+0.0093r3+1-1` (via `Version`)
/// - Calendar versions, like `20250101-1` (via `Opaque`)
/// - Opaque strings, like `abcd` (via `Opaque`)
///
/// As a special case, it also proactively trims leading `v` characters
/// from versions before trying to parse them;
/// this means it can also support strings like `v1.2.3` as though they were `SemVer`.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Documented, ToSchema, Display)]
#[schema(example = json!("v1.0.0"))]
#[display("{}", self.as_str())]
pub enum Revision {
    /// The revision was parseable as a version.
    Version(Version),

    /// The revision is an opaque string.
    #[schema(value_type = String)]
    Opaque(CompactString),
}

impl Revision {
    /// Parse the input string.
    pub fn new(v: impl AsRef<str>) -> Self {
        Self::from(v.as_ref())
    }

    /// Create a new opaque variant from the input without checking
    /// if it should be parsed as an actual version.
    pub fn new_opaque(v: impl Into<CompactString>) -> Self {
        Self::Opaque(v.into())
    }

    /// View the item as a string.
    pub fn as_str(&self) -> &str {
        match self {
            Revision::Version(v) => v.as_str(),
            Revision::Opaque(v) => v.as_str(),
        }
    }
}

impl From<String> for Revision {
    fn from(value: String) -> Self {
        Self::from(value.as_str())
    }
}

impl From<&String> for Revision {
    fn from(value: &String) -> Self {
        Self::from(value.as_str())
    }
}

impl From<&str> for Revision {
    fn from(value: &str) -> Self {
        match Version::parse(value) {
            Some(v) => Self::Version(v),
            None => Self::Opaque(value.to_compact_string()),
        }
    }
}

impl FromStr for Revision {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::from(s))
    }
}

impl From<&Revision> for Revision {
    fn from(value: &Revision) -> Self {
        value.clone()
    }
}

impl Serialize for Revision {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Revision {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        String::deserialize(deserializer).map(Self::from)
    }
}

impl std::cmp::Ord for Revision {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let cmp = alphanumeric_sort::compare_str;
        match (self, other) {
            (Revision::Version(a), Revision::Version(b)) => a.cmp(b),
            (Revision::Version(a), Revision::Opaque(b)) => cmp(a.as_str(), b.as_str()),
            (Revision::Opaque(a), Revision::Version(b)) => cmp(a.as_str(), b.as_str()),
            (Revision::Opaque(a), Revision::Opaque(b)) => cmp(a, b),
        }
    }
}

impl std::cmp::PartialOrd for Revision {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// `LocatorParts` identifies the location of a software package.
///
/// This container type generically describes a locator-shaped concept;
/// users of this library can then customize the types of each interior field
/// to choose the details of how parsing, rendering, comparison, and other
/// operations are performed.
///
/// The intention is to make it possible for users to precisely control
/// what parts of a locator they need, how it is displayed, and how it is parsed.
///
/// ## Higher-order locators
///
/// A "higher-order locator" is a concrete type made up of this type.
/// It's usually implemented as a simple type alias. For example:
/// ```ignore
/// /// `GenericLocator` doesn't care about any parser constraints,
/// /// it just wants to read the data that was passed in by the user
/// /// in its original form (as a `String`) if they provided it at all.
/// type GenericLocator = LocatorParts<
///     Option<String>,
///     Option<String>,
///     Option<String>,
///     Option<String>,
/// >;
/// ```
///
/// As a default, you should strongly consider using a predefined higher-order locator:
/// - [`Locator`] is the most common shape for a locator in FOSSA.
/// - [`StrictLocator`] is like [`Locator`], but requires that the [`Revision`] is present.
/// - [`PackageLocator`] is like [`Locator`], but has no [`Revision`].
///
/// ## Conversions
///
/// This type broadly supports conversions based on the convertability of internal types:
/// - If all interior types are infallibly convertable,
///   `LocatorParts` is infallibly convertable.
/// - If all interior types are a mixture of fallibily and infallibly convertable,
///   `LocatorParts` is fallibly convertable.
/// - If not all (or no) interior types are convertable at all,
///   `LocatorParts` is not convertable either.
///
/// For example, let's say we have two higher-order locators:
/// ```
/// type UnitLocator = LocatorParts<(), (), (), ()>;
/// type ByteLocator = LocatorParts<Vec<u8>, Vec<u8>, Vec<u8>, Vec<u8>>;
/// type StringLocator = LocatorParts<String, String, String, String>;
/// type PartialLocator = LocatorParts<String, Option<String>, String, Option<String>>;
/// ```
///
/// The conversion rules would follow:
/// - `UnitLocator` isn't convertable to or from either other variant,
///   because `std` doesn't implement `From` or `TryFrom` on anything other than itself.
/// - `StringLocator` is infallibly convertable to `ByteLocator`
///    because the standard library blanket implements `impl From<String> for Vec<u8>`.
/// - `ByteLocator` is fallibly convertable to `StringLocator`
///    because the standard library blanket implements `impl TryFrom<Vec<u8>> for String`.
/// - `PartialLocator` is fallibly convertable to `StringLocator`
///    because the standard library blanket implements `impl<T> TryFrom<Option<T>> for T`;
///    even though _some_ fields are infallibly convertable the overall type is not.
/// - `StringLocator` is infallibly convertable to `PartialLocator`
///    because the standard library blanket implements `impl<T> From<T> for Option<T>`.
/// - And finally, `PartialLocator` cannot be converted to or from `ByteLocator` at all-
///   even though `Vec<u8>` and `String` have blanket conversions, the standard library
///   _does not_ propagate blanket implementations through containers like `Option<T>`.
///
/// From here it follows that if you want to use custom types as locator fields,
/// and have those be convertable with other types, you need to implement
/// `TryFrom` or `From` for each type with any other types you want to convert.
///
/// ## Guarantees
///
/// This type represents a _validly-constructed_ locator (depending on the inputs used),
/// but does not guarantee whether a package or revision actually exists or is accessible
/// in the code host.
///
/// ## Ordering
///
/// This type orders by its constituent fields according to the ordering
/// implemented by their types.
///
/// **Important:** there may be other metrics for ordering using the actual code host
/// which contains the package- for example ordering by release date, or code hosts
/// such as `git` which have non-linear history (making flat ordering a lossy operation).
/// This type does not take such edge cases into account in any way.
///
/// ## Rendering
///
/// Locators are canonically rendered to a string before being serialized
/// to the database or sent over the network according to the rules in this section.
///
/// Actual rendering is up to the types being used;
/// this container just renders each field using `serde` and inserts its value
/// into the string in the format below.
///
/// The output string always follows the following formula:
/// ```ignore
/// {ecosystem}+{org_id}/{package}${revision}
/// ```
///
/// With the caveat that:
/// - If a field is optional and `None`, it and its separator are omitted.
/// - Otherwise, the field and its separator are written.
/// - The `$` separator is actually attached to `revision`.
///
/// For example, let's say we have this locator:
/// ```ignore
/// type StringLocator = LocatorParts<String, Option<String>, String, Option<String>>;
/// ```
///
/// Which we'll refer to with the shorthand `parts` macro- for example:
/// ```ignore
/// parts!("npm", None, "lodash", "1.0.0");
/// ```
///
/// Here's some examples of rendering:
/// - `parts!("npm", None, "lodash", "1.0.0")` -> `npm+lodash$1.0.0`
/// - `parts!("npm", Some("1"), "lodash", "1.0.0")` -> `npm+1/lodash$1.0.0`
/// - `parts!("npm", None, "lodash", None)` -> `npm+lodash`
/// - `parts!("npm", Some("1"), "lodash", None)` -> `npm+1/lodash`
/// - `parts!("git", Some("1234"), "github.com/lodash/lodash", "v1.0.0-beta.1")`
///   -> `git+1234/github.com/lodash/lodash$v1.0.0-beta.1`
/// - `parts!("git", None, "github.com/lodash/lodash", "v1.0.0-beta.1")`
///   -> `git+github.com/lodash/lodash$v1.0.0-beta.1`
///
/// Values are serialized directly, not encoded as JSON or whatever,
/// so the value serialized must be a plain string or other basic type.
/// If you want to encode something more complicated, transform it to a string first
/// and then serialize that.
///
/// This type handles optional serialization by skipping any fields that
/// serialize to a blank string. Conveniently, this is the behavior of `Option`
/// with the serializer used by this type.
///
/// ## Parsing
///
/// Locators are canonically rendered to a string before being serialized
/// to the database or sent over the network according to the rules in "rendering";
/// parsing is the inverse of this arrangement.
///
/// Actual parsing is up to the types being used;
/// this container just parses a string into the sections below
/// and then hands each section off to the interior type to be parsed
/// using `serde`.
///
/// The input string must be in one of the following formats:
/// ```ignore
/// {ecosystem}+{package}${revision}
/// {ecosystem}+{package}
/// {ecosystem}+{org_id}/{package}${revision}
/// {ecosystem}+{org_id}/{package}
/// ```
///
/// Note that locators do not feature escaping: instead the _first_ instance
/// of each delimiter (`+`, `/`, `$`) is used to split the fields. However,
/// as a special case organization IDs are only extracted if the field content
/// fully consists of a non-negative integer.
///
/// Parsing examples:
/// - `npm+lodash$1.0.0`
///   - `npm` is extracted and handed to `E` to parse.
///   - There is no org, so a blank string is handed to `O` to parse.
///   - `lodash` is extracted and handed to `P` to parse.
///   - `1.0.0` is extracted and handed to `R` to parse.
/// - `npm+1234/lodash$1.0.0`
///   - `npm` is extracted and handed to `E` to parse.
///   - `1234` is extracted and handed to `O` to parse.
///   - `lodash` is extracted and handed to `P` to parse.
///   - `1.0.0` is extracted and handed to `R` to parse.
/// - `npm+lodash`
///   - `npm` is extracted and handed to `E` to parse.
///   - There is no org, so a blank string is handed to `O` to parse.
///   - `lodash` is extracted and handed to `P` to parse.
///   - There is no revision, so a blank string is handed to `R` to parse.
/// - `npm`
///   - `npm` is extracted and handed to `E` to parse.
///   - There is no org, so a blank string is handed to `O` to parse.
///   - There is no package, so a blank string is handed to `P` to parse.
///   - There is no revision, so a blank string is handed to `R` to parse.
/// - `(blank string)`
///   - There is no ecosystem, so a blank string is handed to `E` to parse.
///   - There is no org, so a blank string is handed to `O` to parse.
///   - There is no package, so a blank string is handed to `P` to parse.
///   - There is no revision, so a blank string is handed to `R` to parse.
/// - `git+github.com/lodash/lodash$v1.0.0`
///   - `git` is extracted and handed to `E` to parse.
///   - There is no org, so a blank string is handed to `O` to parse.
///   - `github.com/lodash/lodash` is extracted and handed to `P` to parse.
///   - `v1.0.0` is extracted and handed to `R` to parse.
///
/// Any parser may error, in which case the overall parse operation errors.
/// Refer to the docs for each part to determine their parsing behavior.
///
/// ## Further Reading
//
// For more information on the background of `Locator` and ecosystems generally,
// FOSSA employees may refer to the "ecosystems and locators" doc: https://go/ecosystems-doc.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Builder, Documented)]
pub struct LocatorParts<E, O, P, R> {
    /// Determines which ecosystem is used to download this package.
    pub ecosystem: E,

    /// Specifies the organization ID to which this package is namespaced.
    ///
    /// Locators are namespaced to an organization when FOSSA needs to use the
    /// private repositories or settings configured by the user to resolve the package.
    ///
    /// Generally, users can treat this as an implementation detail:
    /// Organization IDs namespacing a package means the package should concretely be considered different;
    /// for example `npm+lodash$1.0.0` should be considered different from `npm+1234/lodash$1.0.0`.
    /// The reasoning for this is that private packages may be totally different than
    /// a similarly named public package- in the example above, both of them being `lodash@1.0.0`
    /// doesn't really imply that they are both the popular project known as "lodash".
    /// We know the public one is (`npm+lodash$1.0.0`), but the private one could be anything.
    ///
    /// Examples:
    /// - A public Maven package that is hosted on Maven Central is not namespaced.
    /// - A private Maven package that is hosted on a private host is namespaced.
    /// - A public NPM package that is hosted on NPM is not namespaced.
    /// - A private NPM package that is hosted on NPM but requires credentials is namespaced.
    pub org: O,

    /// Specifies the unique identifier for the package by ecosystem.
    ///
    /// For example, the `git` ecosystem fetching a github package
    /// uses a value in the form of `{user_name}/{package_name}`.
    pub package: P,

    /// Specifies the version for the package by ecosystem.
    ///
    /// For example, the `git` ecosystem fetching a github package
    /// uses a value in the form of `{git_sha}` or `{git_tag}`,
    /// and the ecosystem disambiguates.
    pub revision: R,
}

impl<E, O, P, R> LocatorParts<E, O, P, R> {}

impl<'de, E, O, P, R> Deserialize<'de> for LocatorParts<E, O, P, R>
where
    E: Deserialize<'de>,
    O: Deserialize<'de>,
    P: Deserialize<'de>,
    R: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let input = String::deserialize(deserializer)?;

        let Some((_, ecosystem, package, revision)) = locator_regex!(parse => &input) else {
            return Err(Error::from(ParseError::Syntax { input }))
                .map_err(serde::de::Error::custom);
        };
        let parts = locator_regex!(parse_package => package);
        let (_, org, package) = parts.unwrap_or((package, "", package));

        let ecosystem = E::deserialize(ecosystem.into_deserializer())?;
        let org = O::deserialize(org.into_deserializer())?;
        let package = P::deserialize(package.into_deserializer())?;
        let revision = R::deserialize(revision.into_deserializer())?;

        Ok(Self {
            ecosystem,
            org,
            package,
            revision,
        })
    }
}

impl<E, O, P, R> std::fmt::Display for LocatorParts<E, O, P, R>
where
    E: Serialize,
    O: Serialize,
    P: Serialize,
    R: Serialize,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match serde_plain::to_string(self) {
            Ok(s) => write!(f, "{s}"),
            Err(_) => panic!("LocatorParts failed to serialize display"),
        }
    }
}

impl<E, O, P, R> Serialize for LocatorParts<E, O, P, R>
where
    E: Serialize,
    O: Serialize,
    P: Serialize,
    R: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use serde::ser::Error;
        use serde_plain::to_string;
        use std::fmt::Write;
        let ecosystem = to_string(&self.ecosystem).map_err(Error::custom)?;
        let org = to_string(&self.org).map_err(Error::custom)?;
        let package = to_string(&self.package).map_err(Error::custom)?;
        let revision = to_string(&self.revision).map_err(Error::custom)?;

        let mut output = String::new();
        if !ecosystem.is_empty() {
            write!(&mut output, "{ecosystem}+").map_err(Error::custom)?;
        }
        if !org.is_empty() {
            write!(&mut output, "{org}/").map_err(Error::custom)?;
        }
        if !package.is_empty() {
            write!(&mut output, "{package}").map_err(Error::custom)?;
        }
        if !revision.is_empty() {
            write!(&mut output, "${revision}").map_err(Error::custom)?;
        }

        output.shrink_to_fit();
        output.serialize(serializer)
    }
}

type StrictLocatorT = LocatorParts<Ecosystem, Option<OrgId>, Package, Revision>;
type LocatorT = LocatorParts<Ecosystem, Option<OrgId>, Package, Option<Revision>>;
type PackageLocatorT = LocatorParts<Ecosystem, Option<OrgId>, Package, ()>;

/// Optionally parse an org ID and trimmed package out of a package string.
fn parse_org_package(input: &str) -> (Option<OrgId>, Package) {
    macro_rules! construct {
        ($org_id:expr, $package:expr) => {
            return (Some($org_id), Package::from($package))
        };
        ($package:expr) => {
            return (None, Package::from($package))
        };
    }

    // No `/`? This must not be namespaced.
    let Some((org_id, package)) = input.split_once('/') else {
        construct!(input);
    };

    // Nothing before or after the `/`? Still not namespaced.
    if org_id.is_empty() || package.is_empty() {
        construct!(input);
    };

    // If the part before the `/` isn't a number, it can't be a namespaced org id.
    let Ok(org_id) = org_id.parse() else {
        construct!(input)
    };

    // Ok, there was text before and after the `/`, and the content before was a number.
    // Finally, we've got a namespaced package.
    construct!(org_id, package)
}

/// Create a [`Revision`] in a manner that is known to not fail at compile time.
///
/// ```
/// # use locator::{Constraint, Constraints, Revision, Version};
/// let revision = locator::revision!(1, 0, 0);
/// let expected = Revision::Version(Version::new_semver(1, 0, 0));
/// assert_eq!(revision, expected);
///
/// let revision = locator::revision!("abcd1234");
/// let expected = Revision::new_opaque("abcd1234");
/// assert_eq!(revision, expected);
/// ```
#[macro_export]
macro_rules! revision {
    ($major:expr, $minor:expr, $patch:expr) => {
        $crate::Revision::Version($crate::Version::new_semver($major, $minor, $patch))
    };
    ($input:expr) => {
        $crate::Revision::from($input)
    };

    // This is only meant for use internally, so it's undocumented.
    // Panicks if the provided value isn't semver.
    (parse_semver => $value:literal) => {
        $crate::Revision::Version($crate::Version::parse($value).expect("parse semver"))
    };

    // This is only meant for use internally, so it's undocumented.
    // Forces the provided value to be stored as an opaque string even if it's parseable as a version.
    (parse_opaque => $value:literal) => {
        $crate::Revision::new_opaque($value)
    };
}

#[cfg(test)]
mod tests {
    use simple_test_case::test_case;

    use super::*;

    #[test_case("0/name", Some(OrgId(0)), Package::from("name"); "0/name")]
    #[test_case("1/name", Some(OrgId(1)), Package::from("name"); "1/name")]
    #[test_case("1/name/foo", Some(OrgId(1)), Package::from("name/foo"); "1/name/foo")]
    #[test_case("1//name/foo", Some(OrgId(1)), Package::from("/name/foo"); "doubleslash_1/name/foo")]
    #[test_case("9809572/name/foo", Some(OrgId(9809572)), Package::from("name/foo"); "9809572/name/foo")]
    #[test_case("name/foo", None, Package::from("name/foo"); "name/foo")]
    #[test_case("name", None, Package::from("name"); "name")]
    #[test_case("/name/foo", None, Package::from("/name/foo"); "/name/foo")]
    #[test_case("/123/name/foo", None, Package::from("/123/name/foo"); "/123/name/foo")]
    #[test_case("/name", None, Package::from("/name"); "/name")]
    #[test_case("abcd/1234/name", None, Package::from("abcd/1234/name"); "abcd/1234/name")]
    #[test_case("1abc2/name", None, Package::from("1abc2/name"); "1abc2/name")]
    #[test_case("name/1234", None, Package::from("name/1234"); "name/1234")]
    #[test]
    fn parses_org_package(input: &str, org: Option<OrgId>, package: Package) {
        let (org_id, name) = parse_org_package(input);
        assert_eq!(org_id, org, "'org_id' must match in '{input}'");
        assert_eq!(package, name, "'package' must match in '{input}");
    }

    #[test]
    fn plain_serialization() {
        // Simple option
        let t = serde_plain::to_string(&None::<String>).unwrap();
        pretty_assertions::assert_eq!(t, "");

        // Complex options work too so long as they're `None`
        let t = serde_plain::to_string(&None::<Vec<Vec<()>>>).unwrap();
        pretty_assertions::assert_eq!(t, "");

        // And `Some` works for simple types
        let t = serde_plain::to_string(&Some(String::from("foo"))).unwrap();
        pretty_assertions::assert_eq!(t, "foo");

        let t = serde_plain::to_string(&Some(1usize)).unwrap();
        pretty_assertions::assert_eq!(t, "1");
    }
}
