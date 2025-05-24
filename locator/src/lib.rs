#![doc = include_str!("../../README.md")]
#![deny(unsafe_code)]
#![deny(missing_docs)]
#![warn(rust_2018_idioms)]

use std::str::FromStr;

use derive_more::{Debug, Display};
use documented::Documented;
use duplicate::duplicate;
use nom::{
    Finish, IResult, Parser,
    branch::alt,
    bytes::complete::is_not,
    character::complete::{char, digit1},
    combinator::{opt, rest, success},
    sequence::terminated,
};
use serde::{Deserialize, Serialize, Serializer};

pub mod constraint;
mod error;
mod locator_package;
mod locator_strict;
mod locator_t;
mod org_id;
mod package;
mod revision;

pub use constraint::*;
pub use ecosystems::{Ecosystem, EcosystemPrivate, EcosystemPublic};
pub use error::*;
pub use locator_package::*;
pub use locator_strict::*;
pub use locator_t::*;
pub use org_id::*;
pub use package::*;
pub use revision::*;
use utoipa::ToSchema;

/// Re-exported crates referenced in proc macros.
#[doc(hidden)]
pub mod macro_support {
    pub use bon;
    pub use semver;
    pub use versions;
}

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
    /// Archive locators are FOSSA specific.
    Private => Archive, "archive";
    /// Interacts with Bower.
    Public => Bower, "bower";
    /// Interacts with Carthage.
    Public => Cart, "cart";
    /// Interacts with Cargo.
    Public => Cargo, "cargo";
    /// Interacts with projects from CodeSentry
    Private => CodeSentry, "csbinary";
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
    /// The `custom` ecosystem describes first party projects in FOSSA.
    Private => Custom, "custom";
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
    /// Indicates a specific RPM file.
    Private => Rpm, "rpm";
    /// An unresolved path dependency.
    Private => UnresolvedPath, "upath";
    /// Specifies arbitrary code at an arbitrary URL.
    Public => Url, "url";
    /// A user-specified package.
    Private => User, "user";
)]
pub struct ecosystems;

/// This field indicates no value: it's the equivalent of an always-`None` `Option<()>`.
///
/// - Unconditionally parses from any value.
/// - Unconditionally serializes like a `None::<()>`.
/// - Displays as an empty string.
/// - Always equals itself, hashes to the same value, and compares equally to itself.
#[derive(
    Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display, Documented, ToSchema, Default,
)]
#[display("")]
pub struct Empty;

impl Serialize for Empty {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_none()
    }
}

impl<'de> Deserialize<'de> for Empty {
    fn deserialize<D>(_: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self)
    }
}

impl AsRef<Empty> for Empty {
    fn as_ref(&self) -> &Empty {
        self
    }
}

impl<T> From<&T> for Empty {
    fn from(_: &T) -> Self {
        Self
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
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Documented)]
pub struct LocatorParts<E, O, P, R> {
    #[doc = locator_codegen::field_doc_ecosystem!()]
    pub ecosystem: E,

    #[doc = locator_codegen::field_doc_organization!()]
    pub organization: O,

    #[doc = locator_codegen::field_doc_package!()]
    pub package: P,

    #[doc = locator_codegen::field_doc_revision!()]
    pub revision: R,
}

impl<E, O, P, R> LocatorParts<E, O, P, R> {
    /// Create a new instance.
    pub fn new(
        ecosystem: impl Into<E>,
        organization: impl Into<O>,
        package: impl Into<P>,
        revision: impl Into<R>,
    ) -> Self {
        Self {
            ecosystem: ecosystem.into(),
            organization: organization.into(),
            package: package.into(),
            revision: revision.into(),
        }
    }

    /// Map the value of the `ecosystem` field.
    pub fn map_ecosystem<E2>(self, f: impl FnOnce(E) -> E2) -> LocatorParts<E2, O, P, R> {
        LocatorParts {
            ecosystem: f(self.ecosystem),
            organization: self.organization,
            package: self.package,
            revision: self.revision,
        }
    }

    /// Fallibly map the value of the `ecosystem` field.
    pub fn try_map_ecosystem<E2, X>(
        self,
        f: impl FnOnce(E) -> Result<E2, X>,
    ) -> Result<LocatorParts<E2, O, P, R>, (X, O, P, R)> {
        let (e, o, p, r) = self.into_tuple();
        match f(e) {
            Ok(e) => Ok((e, o, p, r).into()),
            Err(err) => Err((err, o, p, r)),
        }
    }

    /// Map the value of the `organization` field.
    pub fn map_organization<O2>(self, f: impl FnOnce(O) -> O2) -> LocatorParts<E, O2, P, R> {
        LocatorParts {
            ecosystem: self.ecosystem,
            organization: f(self.organization),
            package: self.package,
            revision: self.revision,
        }
    }

    /// Fallibly map the value of the `organization` field.
    pub fn try_map_organization<O2, X>(
        self,
        f: impl FnOnce(O) -> Result<O2, X>,
    ) -> Result<LocatorParts<E, O2, P, R>, (E, X, P, R)> {
        let (e, o, p, r) = self.into_tuple();
        match f(o) {
            Ok(o) => Ok((e, o, p, r).into()),
            Err(err) => Err((e, err, p, r)),
        }
    }

    /// Map the value of the `package` field.
    pub fn map_package<P2>(self, f: impl FnOnce(P) -> P2) -> LocatorParts<E, O, P2, R> {
        LocatorParts {
            ecosystem: self.ecosystem,
            organization: self.organization,
            package: f(self.package),
            revision: self.revision,
        }
    }

    /// Fallibly map the value of the `package` field.
    pub fn try_map_package<P2, X>(
        self,
        f: impl FnOnce(P) -> Result<P2, X>,
    ) -> Result<LocatorParts<E, O, P2, R>, (E, O, X, R)> {
        let (e, o, p, r) = self.into_tuple();
        match f(p) {
            Ok(p) => Ok((e, o, p, r).into()),
            Err(err) => Err((e, o, err, r)),
        }
    }

    /// Map the value of the `revision` field.
    pub fn map_revision<R2>(self, f: impl FnOnce(R) -> R2) -> LocatorParts<E, O, P, R2> {
        LocatorParts {
            ecosystem: self.ecosystem,
            organization: self.organization,
            package: self.package,
            revision: f(self.revision),
        }
    }

    /// Fallibly map the value of the `revision` field.
    pub fn try_map_revision<R2, X>(
        self,
        f: impl FnOnce(R) -> Result<R2, X>,
    ) -> Result<LocatorParts<E, O, P, R2>, (E, O, P, X)> {
        let (e, o, p, r) = self.into_tuple();
        match f(r) {
            Ok(r) => Ok((e, o, p, r).into()),
            Err(err) => Err((e, o, p, err)),
        }
    }

    /// Explode the parts into a tuple.
    pub fn into_tuple(self) -> (E, O, P, R) {
        (
            self.ecosystem,
            self.organization,
            self.package,
            self.revision,
        )
    }

    /// Convert a tuple of parts back to self.
    pub fn from_tuple((e, o, p, r): (E, O, P, R)) -> Self {
        Self::new(e, o, p, r)
    }
}

impl<E, O, P, R> LocatorParts<E, O, P, R>
where
    for<'g> E: Deserialize<'g>,
    for<'g> O: Deserialize<'g>,
    for<'g> P: Deserialize<'g>,
    for<'g> R: Deserialize<'g>,
{
    /// `nom` parser to parse an instance from the string.
    /// For details, see the `Parsing` section on the type documentation.
    pub fn parse(input: impl AsRef<str>) -> Result<Self, Error> {
        /// `nom` parser for the locator fields.
        fn parse_fields(s: &str) -> IResult<&str, (&str, &str, &str, &str)> {
            let (s, ecosystem) = terminated(is_not("+"), char('+')).parse(s)?;
            let (s, org) = alt((terminated(digit1, char('/')), success(""))).parse(s)?;
            let (s, package) = alt((terminated(is_not("$"), char('$')), is_not("$"))).parse(s)?;
            let (s, revision) = opt(rest).parse(s)?;
            Ok((s, (ecosystem, org, package, revision.unwrap_or(""))))
        }

        /// Convenience macro to deserialize a single field using serde,
        /// wrapping the returned error into the error type of this module.
        macro_rules! de_field {
            ($input:expr => $ty:ident, $field:expr, $fragment:expr) => {
                serde_plain::from_str($fragment)
                    .map_err(|err| error::field!($input.clone(), $field => span($input, $fragment), err.clone()))
            };
        }

        // Do the actual parsing!
        let input = input.as_ref();
        Ok(match parse_fields.parse_complete(input.trim()).finish() {
            Ok((_, (eco, org, pkg, rev))) => Self {
                ecosystem: de_field!(input => E, "ecosystem", eco)?,
                organization: de_field!(input => O, "organization", org)?,
                package: de_field!(input => P, "package", pkg)?,
                revision: de_field!(input => R, "revision", rev)?,
            },
            Err(err) => {
                let err = error::syntax!(input => span(input, err.input), err.cloned());
                return Err(Error::Parse(err));
            }
        })
    }
}

impl<'de, E, O, P, R> Deserialize<'de> for LocatorParts<E, O, P, R>
where
    for<'g> E: Deserialize<'g>,
    for<'g> O: Deserialize<'g>,
    for<'g> P: Deserialize<'g>,
    for<'g> R: Deserialize<'g>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let input = String::deserialize(deserializer)?;
        Self::parse(&input).map_err(serde::de::Error::custom)
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
        let org = to_string(&self.organization).map_err(Error::custom)?;
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

impl<E, O, P, R> FromStr for LocatorParts<E, O, P, R>
where
    for<'g> E: Deserialize<'g>,
    for<'g> O: Deserialize<'g>,
    for<'g> P: Deserialize<'g>,
    for<'g> R: Deserialize<'g>,
{
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s)
    }
}

duplicate! {
    [
        ty;
        [ &str ];
        [ &String ];
        [ String ];
    ]
    impl<E, O, P, R> TryFrom<ty> for LocatorParts<E, O, P, R>
    where
        for<'g> E: Deserialize<'g>,
        for<'g> O: Deserialize<'g>,
        for<'g> P: Deserialize<'g>,
        for<'g> R: Deserialize<'g>,
    {
        type Error = Error;
        fn try_from(s: ty) -> Result<Self, Self::Error> {
            Self::parse(s)
        }
    }
}

impl<E, O, P, R> From<(E, O, P, R)> for LocatorParts<E, O, P, R> {
    fn from(value: (E, O, P, R)) -> Self {
        Self::from_tuple(value)
    }
}

impl<E, O, P, R> From<LocatorParts<E, O, P, R>> for (E, O, P, R) {
    fn from(value: LocatorParts<E, O, P, R>) -> Self {
        value.into_tuple()
    }
}

impl<E, O, P, R> From<&LocatorParts<E, O, P, R>> for LocatorParts<E, O, P, R>
where
    E: Clone,
    O: Clone,
    P: Clone,
    R: Clone,
{
    fn from(value: &LocatorParts<E, O, P, R>) -> Self {
        value.clone()
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::LocatorParts;

    #[test]
    fn cloned() {
        type StringLocator = LocatorParts<String, String, String, String>;
        let a = StringLocator::new("eco", "org", "pkg", "rev");
        let b = StringLocator::from(&a);
        assert_eq!(a, b);
    }

    #[test]
    fn cloned_newtype() {
        #[derive(Clone, Eq, PartialEq, Debug)]
        struct StringLocator(LocatorParts<String, String, String, String>);
        let a = StringLocator(LocatorParts::new("eco", "org", "pkg", "rev"));
        let b = StringLocator::from(a.clone());
        assert_eq!(a, b);
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
