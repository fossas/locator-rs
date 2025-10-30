use std::{borrow::Cow, str::FromStr};

use compact_str::{CompactString, ToCompactString, format_compact};
use derivative::Derivative;
use derive_more::{Debug, Display};
use documented::Documented;
use lazy_regex::regex_is_match;
use non_empty_string::NonEmptyString;
use serde::{Deserialize, Serialize};
use serde_json::json;
use utoipa::{
    PartialSchema, ToSchema,
    openapi::{ObjectBuilder, Type},
};
use versions::Versioning;

use crate::{Error, ParseError, error};

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
    pub(crate) parsed: Versioning,

    /// The original input.
    ///
    /// Stored so that:
    /// - We can cheaply reference it when doing string comparisons
    /// - We can use the input value instead of the parsed value when printing
    ///   (since the parsed value may be different, e.g. if there's a `v` prefix).
    #[derivative(PartialEq = "ignore", Ord = "ignore", Hash = "ignore")]
    pub(crate) input: CompactString,
}

// Intuitively we'd expect that if we have an already parsed semver::Version, then we should be able
// to create an instance of Version without it failing. To facilitate this we work around
// slight differences in the types to provide a conversion that will always succeed.
impl From<semver::Version> for Version {
    fn from(value: semver::Version) -> Self {
        // We only reparse the semver using the versions library to extract the pre-release chunks
        // For the rest of the fields we re-use the parsed values that the semver provides
        let converted = versions::SemVer::new(&value.to_string());
        Self {
            input: value.to_compact_string(),
            parsed: Versioning::Ideal(versions::SemVer {
                major: u32::try_from(value.major).unwrap_or(u32::MAX),
                minor: u32::try_from(value.minor).unwrap_or(u32::MAX),
                patch: u32::try_from(value.patch).unwrap_or(u32::MAX),
                meta: if value.build.is_empty() {
                    None
                } else {
                    Some(value.build.to_string())
                },
                pre_rel: if value.pre.is_empty() {
                    None
                } else {
                    match converted {
                        Some(v) => v.pre_rel,
                        _ => None,
                    }
                },
            }),
        }
    }
}

impl From<&semver::Version> for Version {
    fn from(value: &semver::Version) -> Self {
        Self::from(value.clone())
    }
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
    pub fn parse(v: impl AsRef<str>) -> Result<Self, Error> {
        v.as_ref().try_into()
    }

    /// View the item as a string.
    pub fn as_str(&self) -> &str {
        match self {
            Revision::Version(v) => v.as_str(),
            Revision::Opaque(v) => v.as_str(),
        }
    }
}

impl From<NonEmptyString> for Revision {
    fn from(value: NonEmptyString) -> Self {
        let value = value.as_str();
        match Version::parse(value) {
            Some(v) => Self::Version(v),
            None => Self::Opaque(value.to_compact_string()),
        }
    }
}

impl From<&NonEmptyString> for Revision {
    fn from(value: &NonEmptyString) -> Self {
        let value = value.as_str();
        match Version::parse(value) {
            Some(v) => Self::Version(v),
            None => Self::Opaque(value.to_compact_string()),
        }
    }
}

impl TryFrom<String> for Revision {
    type Error = <Revision as FromStr>::Err;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        <Revision as FromStr>::from_str(&value)
    }
}

impl TryFrom<&String> for Revision {
    type Error = <Revision as FromStr>::Err;

    fn try_from(value: &String) -> Result<Self, Self::Error> {
        <Revision as FromStr>::from_str(value)
    }
}

impl TryFrom<&str> for Revision {
    type Error = <Revision as FromStr>::Err;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        <Revision as FromStr>::from_str(value)
    }
}

impl FromStr for Revision {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        if s.is_empty() {
            error::fatal!(ParseError::Empty)
        }
        Ok(match Version::parse(s) {
            Some(v) => Self::Version(v),
            None => Self::Opaque(s.to_compact_string()),
        })
    }
}

impl From<&Revision> for Revision {
    fn from(value: &Revision) -> Self {
        value.clone()
    }
}

impl From<semver::Version> for Revision {
    fn from(value: semver::Version) -> Self {
        Self::Version(value.into())
    }
}

impl From<&semver::Version> for Revision {
    fn from(value: &semver::Version) -> Self {
        Self::Version(value.clone().into())
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
        let s = String::deserialize(deserializer)?;
        Self::try_from(s).map_err(serde::de::Error::custom)
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

/// Create a [`Revision`] in a manner that is known to not fail at compile time.
///
/// ```
/// # use locator::{Revision, Version};
/// # use compact_str::ToCompactString;
/// let revision = locator::revision!(1, 0, 0);
/// let expected = Revision::Version(Version::new_semver(1, 0, 0));
/// assert_eq!(revision, expected);
///
/// let revision = locator::revision!("abcd1234");
/// let expected = Revision::Opaque("abcd1234".to_compact_string());
/// assert_eq!(revision, expected);
/// ```
#[macro_export]
macro_rules! revision {
    ($major:expr, $minor:expr, $patch:expr) => {
        $crate::Revision::Version($crate::Version::new_semver($major, $minor, $patch))
    };
    ($input:expr) => {
        $crate::Revision::from($crate::macro_support::non_empty_string::non_empty_string!(
            $input
        ))
    };

    // This is only meant for use internally, so it's undocumented.
    // Panicks if the provided value fails to parse.
    (parse => $value:expr) => {
        $crate::Revision::try_from($value).expect("parse revision")
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
