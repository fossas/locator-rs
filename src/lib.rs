#![doc = include_str!("../README.md")]
#![deny(unsafe_code)]
#![deny(missing_docs)]
#![warn(rust_2018_idioms)]

use std::{borrow::Cow, str::FromStr};

use documented::Documented;
use duplicate::duplicate;
use lazy_static::lazy_static;
use regex::Regex;
use serde::{Deserialize, Serialize};
use strum::{AsRefStr, Display, EnumIter, EnumString};
use utoipa::ToSchema;

mod error;
mod locator;
mod locator_package;
mod locator_strict;

pub use error::*;

pub use locator::*;
pub use locator_package::*;
pub use locator_strict::*;

/// [`Locator`](crate::Locator) is closely tied with the concept of Core's "fetchers",
/// which are asynchronous jobs tasked with downloading the code
/// referred to by a [`Locator`](crate::Locator) so that Core or some other service
/// may analyze it.
///
/// For more information on the background of `Locator` and fetchers generally,
/// refer to [Fetchers and Locators](https://go/fetchers-doc).
#[derive(
    Copy,
    Clone,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Debug,
    Display,
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
pub enum Fetcher {
    /// Archive locators are FOSSA specific.
    #[strum(serialize = "archive")]
    Archive,

    /// Interacts with Bower.
    #[strum(serialize = "bower")]
    Bower,

    /// Interacts with Carthage.
    #[strum(serialize = "cart")]
    Cart,

    /// Interacts with Cargo.
    #[strum(serialize = "cargo")]
    Cargo,

    /// Interacts with Composer.
    #[strum(serialize = "comp")]
    Comp,

    /// Interacts with Conan.
    #[strum(serialize = "conan")]
    Conan,

    /// Interacts with Conda.
    #[strum(serialize = "conda")]
    Conda,

    /// Interacts with CPAN.
    #[strum(serialize = "cpan")]
    Cpan,

    /// Interacts with CRAN.
    #[strum(serialize = "cran")]
    Cran,

    /// The `custom` fetcher describes first party projects in FOSSA.
    ///
    /// These projects aren't really _fetched_;
    /// they're stored in FOSSA's database.
    #[strum(serialize = "custom")]
    Custom,

    /// Interacts with RubyGems.
    #[strum(serialize = "gem")]
    Gem,

    /// Interacts with git VCS hosts.
    #[strum(serialize = "git")]
    Git,

    /// Resolves 'git' dependencies in the same manner as Go modules.
    #[strum(serialize = "go")]
    Go,

    /// Interacts with Hackage.
    #[strum(serialize = "hackage")]
    Hackage,

    /// Interacts with Hex.
    #[strum(serialize = "hex")]
    Hex,

    /// Linux Alpine packages.
    #[strum(serialize = "apk")]
    #[serde(rename = "apk")]
    LinuxAlpine,

    /// Linux Debian packages.
    #[strum(serialize = "deb")]
    #[serde(rename = "deb")]
    LinuxDebian,

    /// Linux RPM packages.
    #[strum(serialize = "rpm-generic")]
    #[serde(rename = "rpm-generic")]
    LinuxRpm,

    /// Interacts with Maven.
    #[strum(serialize = "mvn")]
    Maven,

    /// Interacts with NPM.
    #[strum(serialize = "npm")]
    Npm,

    /// Interacts with Nuget.
    #[strum(serialize = "nuget")]
    Nuget,

    /// Interacts with PyPI.
    #[strum(serialize = "pip")]
    Pip,

    /// Interacts with CocoaPods.
    #[strum(serialize = "pod")]
    Pod,

    /// Interacts with Dart's package manager.
    #[strum(serialize = "pub")]
    Pub,

    /// Indicates RPM files.
    #[strum(serialize = "rpm")]
    Rpm,

    /// Interact with Swift's package manager.
    #[strum(serialize = "swift")]
    Swift,

    /// Specifies an arbitrary URL,
    /// which is downloaded and treated like an `Archive` variant.
    #[strum(serialize = "url")]
    Url,

    /// An unresolved path dependency.
    #[strum(serialize = "upath")]
    #[serde(rename = "upath")]
    UnresolvedPath,

    /// A user-specified package.
    #[strum(serialize = "user")]
    User,
}

/// Identifies the organization to which this locator is namespaced.
///
/// Organization IDs are canonically created by FOSSA instances
/// and have no meaning outside of FOSSA instances.
#[derive(
    Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize, Hash, Documented, ToSchema,
)]
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

impl TryFrom<&str> for OrgId {
    type Error = <usize as FromStr>::Err;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(OrgId(value.parse()?))
    }
}

impl std::fmt::Display for OrgId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Debug for OrgId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

/// The package section of the locator.
#[derive(Clone, Eq, PartialEq, Hash, Serialize, Deserialize, Documented, ToSchema)]
pub struct Package(String);

impl Package {
    /// View the item as a string.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for Package {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<&str> for Package {
    fn from(value: &str) -> Self {
        Self::from(value.to_string())
    }
}

impl From<&Package> for Package {
    fn from(value: &Package) -> Self {
        value.clone()
    }
}

impl std::fmt::Display for Package {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Debug for Package {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
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

/// The revision section of the locator.
#[derive(Clone, Eq, PartialEq, Hash, Documented, ToSchema)]
#[schema(examples(json!("1.0.0"), json!("2.0.0-alpha.1"), json!("abcd1234")))]
pub enum Revision {
    /// The revision is valid semver.
    #[schema(value_type = String)]
    Semver(semver::Version),

    /// The revision is an opaque string.
    Opaque(String),
}

impl Revision {
    /// View the item as a string.
    pub fn as_str(&self) -> Cow<'_, str> {
        match self {
            Revision::Semver(v) => Cow::Owned(v.to_string()),
            Revision::Opaque(v) => Cow::Borrowed(v),
        }
    }
}

impl From<String> for Revision {
    fn from(value: String) -> Self {
        match semver::Version::parse(&value) {
            Ok(v) => Self::Semver(v),
            Err(_) => Self::Opaque(value),
        }
    }
}

impl From<&str> for Revision {
    fn from(value: &str) -> Self {
        Self::from(value.to_string())
    }
}

impl From<&Revision> for Revision {
    fn from(value: &Revision) -> Self {
        value.clone()
    }
}

impl std::fmt::Display for Revision {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Revision::Semver(v) => write!(f, "{v}"),
            Revision::Opaque(v) => write!(f, "{v}"),
        }
    }
}

impl std::fmt::Debug for Revision {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
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
            (Revision::Semver(a), Revision::Semver(b)) => a.cmp(b),
            (Revision::Semver(a), Revision::Opaque(b)) => cmp(&a.to_string(), b),
            (Revision::Opaque(a), Revision::Semver(b)) => cmp(a, &b.to_string()),
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
fn parse_org_package(package: &str) -> Result<(Option<OrgId>, Package), PackageParseError> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^(?:(?P<org_id>\d+)/)?(?P<package>.+)")
            .expect("Package parsing expression must compile");
    }

    let mut captures = RE.captures_iter(package);
    let capture = captures.next().ok_or_else(|| PackageParseError::Package {
        package: package.to_string(),
    })?;

    let trimmed_package =
        capture
            .name("package")
            .map(|m| m.as_str())
            .ok_or_else(|| PackageParseError::Field {
                package: package.to_string(),
                field: String::from("package"),
            })?;

    // If we fail to parse the org_id as a valid number, don't fail the overall parse;
    // just don't namespace to org ID and return the input unmodified.
    match capture
        .name("org_id")
        .map(|m| m.as_str())
        .map(OrgId::try_from)
    {
        // An org ID was provided and validly parsed, use it.
        Some(Ok(org_id)) => Ok((Some(org_id), Package::from(trimmed_package))),

        // Otherwise, if we either didn't get an org ID section,
        // or it wasn't a valid org ID,
        // just use the package as-is.
        _ => Ok((None, Package::from(package))),
    }
}

#[cfg(test)]
mod tests {
    use simple_test_case::test_case;

    use super::*;

    impl Package {
        fn new(value: &str) -> Self {
            Self(value.to_string())
        }
    }

    macro_rules! revision {
        (semver => $input:expr) => {
            Revision::Semver(semver::Version::parse($input).expect("parse semver"))
        };
        (opaque => $input:expr) => {
            Revision::Opaque(String::from($input))
        };
    }

    #[test_case(Some(OrgId(0)), Package::new("name"); "0/name")]
    #[test_case(Some(OrgId(1)), Package::new("name"); "1/name")]
    #[test_case(Some(OrgId(1)), Package::new("name/foo"); "1/name/foo")]
    #[test_case(Some(OrgId(9809572)), Package::new("name/foo"); "9809572/name/foo")]
    #[test_case(None, Package::new("name/foo"); "name/foo")]
    #[test_case(None, Package::new("name"); "name")]
    #[test_case(None, Package::new("/name/foo"); "/name/foo")]
    #[test_case(None, Package::new("/name"); "/name")]
    #[test_case(None, Package::new("abcd/1234/name"); "abcd/1234/name")]
    #[test_case(None, Package::new("1abc2/name"); "1abc2/name")]
    #[test]
    fn parse_org_package(org: Option<OrgId>, package: Package) {
        let test = match org {
            Some(id) => format!("{id}/{package}"),
            None => format!("{package}"),
        };
        let Ok((org_id, name)) = parse_org_package(&test) else {
            panic!("must parse '{test}'")
        };
        assert_eq!(org_id, org, "'org_id' must match in '{test}'");
        assert_eq!(package, name, "'package' must match in '{test}");
    }

    #[test_case(r#""rpm-generic""#, Fetcher::LinuxRpm; "rpm-generic")]
    #[test_case(r#""deb""#, Fetcher::LinuxDebian; "deb")]
    #[test_case(r#""apk""#, Fetcher::LinuxAlpine; "apk")]
    #[test]
    fn serializes_linux_properly(expected: &str, value: Fetcher) {
        assert_eq!(expected, serde_json::to_string(&value).unwrap());
    }

    #[test_case(Package::new("name"); "name")]
    #[test_case(Package::new("name/foo"); "name/foo")]
    #[test_case(Package::new("/name/foo"); "/name/foo")]
    #[test_case(Package::new("/name"); "/name")]
    #[test_case(Package::new("abcd/1234/name"); "abcd/1234/name")]
    #[test_case(Package::new("1abc2/name"); "1abc2/name")]
    #[test]
    fn package_roundtrip(package: Package) {
        let serialized = serde_json::to_string(&package).expect("must serialize");
        let deserialized = serde_json::from_str(&serialized).expect("must deserialize");
        assert_eq!(package, deserialized);
    }

    #[test_case("1.0.0", revision!(semver => "1.0.0"); "1.0.0")]
    #[test_case("1.2.0", revision!(semver => "1.2.0"); "1.2.0")]
    #[test_case("1.0.0-alpha.1", revision!(semver => "1.0.0-alpha.1"); "1.0.0-alpha.1")]
    #[test_case("1.0.0-alpha1", revision!(semver => "1.0.0-alpha1"); "1.0.0-alpha1")]
    #[test_case("1.0.0-rc.10+r1234", revision!(semver => "1.0.0-rc.10+r1234"); "1.0.0-rc.10+r1234")]
    #[test_case("abcd1234", revision!(opaque => "abcd1234"); "abcd1234")]
    #[test_case("v1.0.0", revision!(opaque => "v1.0.0"); "v1.0.0")]
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
