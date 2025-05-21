#![doc = include_str!("../README.md")]
#![deny(unsafe_code)]
#![deny(missing_docs)]
#![warn(rust_2018_idioms)]

use std::{borrow::Cow, convert::Infallible, num::ParseIntError, str::FromStr};

use compact_str::{CompactString, ToCompactString, format_compact};
use derivative::Derivative;
use derive_more::{Debug, Display};
use documented::Documented;
use duplicate::duplicate;
use lazy_regex::regex_is_match;
use serde::{Deserialize, Serialize};
use serde_json::json;
use strum::{AsRefStr, Display as EnumDisplay, EnumIter, EnumString, IntoEnumIterator};
use subenum::subenum;
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

pub use error::*;

pub use constraint::*;
pub use locator::*;
pub use locator_package::*;
pub use locator_strict::*;

#[doc(hidden)]
pub use semver;
#[doc(hidden)]
pub use versions;

/// Identifies supported code host protocols.
///
/// ## [`ProtocolPublic`]
///
/// Most listed protocols are public protocols.
///
/// For example:
/// - `Npm` implies "uses the NPM protocol", meaning the code referenced is distributed with the NPM package manager.
/// - `Git` implies "uses the git protocol", meaning the code referenced is distributed with a git server.
///
/// ## [`ProtocolPrivate`]
///
/// Most protocols are "public", meaning they're not FOSSA controlled.
/// However, some protocols are FOSSA-specific; these mean nothing
/// outside of the context of FOSSA.
///
/// For example:
/// - `Archive` is an indicator for an `archive` project in FOSSA, which is a blob of uploaded source code.
/// - `Custom` is used for top-level projects in FOSSA (not all top-level projects use custom, but custom always means this).
#[subenum(ProtocolPublic, ProtocolPrivate)]
#[derive(
    Copy,
    Clone,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Debug,
    EnumDisplay,
    EnumString,
    EnumIter,
    AsRefStr,
    Serialize,
    Deserialize,
    Documented,
    ToSchema,
)]
#[non_exhaustive]
#[serde(rename_all = "snake_case")]
#[schema(example = json!("git"))]
pub enum Protocol {
    /// Archive locators are FOSSA specific.
    #[subenum(ProtocolPrivate)]
    #[strum(serialize = "archive")]
    Archive,

    /// Interacts with Bower.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "bower")]
    Bower,

    /// Interacts with Carthage.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "cart")]
    Cart,

    /// Interacts with Cargo.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "cargo")]
    Cargo,

    /// Interacts with projects from CodeSentry
    #[subenum(ProtocolPrivate)]
    #[strum(serialize = "csbinary")]
    #[serde(rename = "csbinary")]
    CodeSentry,

    /// Interacts with Composer.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "comp")]
    Comp,

    /// Interacts with Conan.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "conan")]
    Conan,

    /// Interacts with Conda.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "conda")]
    Conda,

    /// Interacts with CPAN.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "cpan")]
    Cpan,

    /// Interacts with CRAN.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "cran")]
    Cran,

    /// The `custom` protocol describes first party projects in FOSSA.
    #[subenum(ProtocolPrivate)]
    #[strum(serialize = "custom")]
    Custom,

    /// Interacts with RubyGems.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "gem")]
    Gem,

    /// Interacts with git VCS hosts.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "git")]
    Git,

    /// Interacts with Go projects.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "go")]
    Go,

    /// Interacts with Hackage.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "hackage")]
    Hackage,

    /// Interacts with Hex.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "hex")]
    Hex,

    /// Interacts with Linux Alpine package managers.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "apk")]
    #[serde(rename = "apk")]
    LinuxAlpine,

    /// Interacts with Linux Debian package managers.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "deb")]
    #[serde(rename = "deb")]
    LinuxDebian,

    /// Interacts with Linux RPM package managers.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "rpm-generic")]
    #[serde(rename = "rpm-generic")]
    LinuxRpm,

    /// Interacts with Maven.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "mvn")]
    Maven,

    /// Interacts with NPM.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "npm")]
    Npm,

    /// Interacts with Nuget.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "nuget")]
    Nuget,

    /// Interacts with PyPI.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "pip")]
    Pip,

    /// Interacts with CocoaPods.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "pod")]
    Pod,

    /// Interacts with Dart's package manager.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "pub")]
    Pub,

    /// Indicates a specific RPM file.
    ///
    /// This is part of the `ProtocolPrivate` enum because while RPMs aren't a concept unique to FOSSA,
    /// the use of this protocol is pretty much meaningless outside of a FOSSA instance.
    ///
    /// Note: this variant only exists for backwards compatibility, you almost definitely mean `LinuxRpm`.
    #[subenum(ProtocolPrivate)]
    #[strum(serialize = "rpm")]
    Rpm,

    /// Interact with Swift's package manager.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "swift")]
    Swift,

    /// Specifies arbitrary code at an arbitrary URL.
    #[subenum(ProtocolPublic)]
    #[strum(serialize = "url")]
    Url,

    /// An unresolved path dependency.
    #[subenum(ProtocolPrivate)]
    #[strum(serialize = "upath")]
    #[serde(rename = "upath")]
    UnresolvedPath,

    /// A user-specified package.
    #[subenum(ProtocolPrivate)]
    #[strum(serialize = "user")]
    User,
}

// Allow iteration over variants without callers needing to install `strum`.
duplicate! {
    [
        ty;
        [ Protocol ];
        [ ProtocolPrivate ];
        [ ProtocolPublic ];
    ]
    impl ty {
        /// Iterate over all variants.
        pub fn iter() -> impl Iterator<Item = ty> {
            <Self as IntoEnumIterator>::iter()
        }
    }
}

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
/// However some protocol protocols (such as `git`) embed additional information
/// inside the `Package` of a locator, such as the URL of the `git` instance
/// from which the project can be fetched.
///
/// Additionally, some protocol protocols (such as `apk`, `rpm-generic`, and `deb`)
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
/// Some protocol protocols (such as `apk`, `rpm-generic`, and `deb`)
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

    #[test_case(r#""rpm-generic""#, Protocol::LinuxRpm; "rpm-generic")]
    #[test_case(r#""deb""#, Protocol::LinuxDebian; "deb")]
    #[test_case(r#""apk""#, Protocol::LinuxAlpine; "apk")]
    #[test]
    fn serializes_linux_properly(expected: &str, value: Protocol) {
        assert_eq!(expected, serde_json::to_string(&value).unwrap());
    }

    #[test_case(Package::from("name"); "name")]
    #[test_case(Package::from("name/foo"); "name/foo")]
    #[test_case(Package::from("/name/foo"); "/name/foo")]
    #[test_case(Package::from("/name"); "/name")]
    #[test_case(Package::from("abcd/1234/name"); "abcd/1234/name")]
    #[test_case(Package::from("1abc2/name"); "1abc2/name")]
    #[test]
    fn package_roundtrip(package: Package) {
        let serialized = serde_json::to_string(&package).expect("must serialize");
        let deserialized = serde_json::from_str(&serialized).expect("must deserialize");
        assert_eq!(package, deserialized);
    }

    #[test_case("1.0.0", revision!(parse_semver => "1.0.0"); "1.0.0")]
    #[test_case("1.2.0", revision!(parse_semver => "1.2.0"); "1.2.0")]
    #[test_case("1.0.0-alpha.1", revision!(parse_semver => "1.0.0-alpha.1"); "1.0.0-alpha.1")]
    #[test_case("1.0.0-alpha1", revision!(parse_semver => "1.0.0-alpha1"); "1.0.0-alpha1")]
    #[test_case("1.0.0-rc.10+r1234", revision!(parse_semver => "1.0.0-rc.10+r1234"); "1.0.0-rc.10+r1234")]
    #[test_case("abcd1234", revision!("abcd1234"); "abcd1234")]
    #[test_case("v1.0.0", revision!("v1.0.0"); "v1.0.0")]
    #[test]
    fn revision(revision: &str, expected: Revision) {
        let serialized = serde_json::to_string(&revision).expect("must serialize");
        let deserialized = serde_json::from_str(&serialized).expect("must deserialize");
        assert_eq!(expected, deserialized);
    }

    #[test_case(Revision::from("1.0.0"); "1.0.0")]
    #[test_case(Revision::from("1.2.0"); "1.2.0")]
    #[test_case(Revision::from("1.0.0-alpha.1"); "1.0.0-alpha.1")]
    #[test_case(Revision::from("1.0.0-alpha1"); "1.0.0-alpha1")]
    #[test_case(Revision::from("1.0.0-rc.10"); "1.0.0-rc.10")]
    #[test_case(Revision::from("abcd1234"); "abcd1234")]
    #[test_case(Revision::from("v1.0.0"); "v1.0.0")]
    #[test]
    fn revision_roundtrip(revision: Revision) {
        let serialized = serde_json::to_string(&revision).expect("must serialize");
        let deserialized = serde_json::from_str(&serialized).expect("must deserialize");
        assert_eq!(revision, deserialized);
    }

    #[test_case(OrgId(1); "1")]
    #[test_case(OrgId(0); "0")]
    #[test_case(OrgId(1210931039); "1210931039")]
    #[test]
    fn org_roundtrip(org: OrgId) {
        let serialized = serde_json::to_string(&org).expect("must serialize");
        let deserialized = serde_json::from_str(&serialized).expect("must deserialize");
        assert_eq!(org, deserialized);
    }
}
