use std::{fmt::Display, str::FromStr};

use documented::Documented;
use getset::{CopyGetters, Getters};
use serde::{Deserialize, Serialize};
use serde_json::json;
use typed_builder::TypedBuilder;
use utoipa::{
    openapi::{ObjectBuilder, SchemaType},
    ToSchema,
};

use crate::{Error, Fetcher, Locator, OrgId, Package, PackageLocator, ParseError, Revision};

/// Convenience macro for creating a [`StrictLocator`].
/// Required types and fields are checked at compile time.
///
/// ```
/// let loc = locator::strict!(Npm, "lodash", "1.0.0");
/// assert_eq!("npm+lodash$1.0.0", &loc.to_string());
///
/// let loc = locator::strict!(org 1234 => Npm, "lodash", "1.0.0");
/// assert_eq!("npm+1234/lodash$1.0.0", &loc.to_string());
/// ```
#[macro_export]
macro_rules! strict {
    (org $org:expr => $fetcher:ident, $package:expr, $version:expr) => {
        $crate::StrictLocator::builder()
            .fetcher($crate::Fetcher::$fetcher)
            .package($package)
            .org_id($org)
            .revision($version)
            .build()
    };
    ($fetcher:ident, $package:expr, $version:expr) => {
        $crate::StrictLocator::builder()
            .fetcher($crate::Fetcher::$fetcher)
            .package($package)
            .revision($version)
            .build()
    };
}

/// A [`Locator`] specialized to **require** the `revision` component.
///
/// ## Ordering
///
/// Locators order by:
/// 1. Fetcher, alphanumerically.
/// 2. Organization ID, alphanumerically; missing organizations are sorted higher.
/// 3. The package field, alphanumerically.
/// 4. The revision field:
///    If both comparing locators use semver, these are compared using semver rules;
///    otherwise these are compared alphanumerically.
///
/// Importantly, there may be other metrics for ordering using the actual code host
/// which contains the package (for example, ordering by release date).
/// This library does not perform such ordering.
///
/// ## Parsing
///
/// The input string must be in the following format:
/// ```ignore
/// {fetcher}+{package}${revision}
/// ```
///
/// Packages may also be namespaced to a specific organization;
/// in such cases the organization ID is at the start of the `{package}` field
/// separated by a slash. The ID can be any non-negative integer.
/// This yields the following format:
/// ```ignore
/// {fetcher}+{org_id}/{package}${revision}
/// ```
#[derive(
    Clone,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Debug,
    TypedBuilder,
    Getters,
    CopyGetters,
    Documented,
)]
pub struct StrictLocator {
    /// Determines which fetcher is used to download this package.
    #[getset(get_copy = "pub")]
    fetcher: Fetcher,

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
    #[builder(default, setter(transform = |id: usize| Some(OrgId(id))))]
    #[getset(get_copy = "pub")]
    org_id: Option<OrgId>,

    /// Specifies the unique identifier for the package by fetcher.
    ///
    /// For example, the `git` fetcher fetching a github package
    /// uses a value in the form of `{user_name}/{package_name}`.
    #[builder(setter(transform = |package: impl ToString| Package(package.to_string())))]
    #[getset(get = "pub")]
    package: Package,

    /// Specifies the version for the package by fetcher.
    ///
    /// For example, the `git` fetcher fetching a github package
    /// uses a value in the form of `{git_sha}` or `{git_tag}`,
    /// and the fetcher disambiguates.
    #[builder(setter(transform = |revision: impl ToString| Revision::from(revision.to_string())))]
    #[getset(get = "pub")]
    revision: Revision,
}

impl StrictLocator {
    /// Parse a `StrictLocator`.
    /// For details, see the parsing section on [`StrictLocator`].
    pub fn parse(locator: &str) -> Result<Self, Error> {
        let (fetcher, org_id, package, revision) = Locator::parse(locator)?.explode();

        let Some(revision) = revision else {
            return Err(Error::Parse(ParseError::Field {
                input: locator.to_owned(),
                field: String::from("revision"),
            }));
        };

        Ok(Self {
            fetcher,
            org_id,
            package,
            revision,
        })
    }

    /// Converts the instance into a [`PackageLocator`] by discarding the `revision` component.
    /// Equivalent to the `From` implementation, but offered as a method for convenience.
    pub fn into_package(self) -> PackageLocator {
        self.into()
    }

    /// Converts the instance into a [`Locator`].
    /// Equivalent to the `From` implementation, but offered as a method for convenience.
    pub fn into_locator(self) -> Locator {
        self.into()
    }

    /// Explodes the locator into its (owned) parts.
    /// Used for conversions without cloning.
    pub(crate) fn explode(self) -> (Fetcher, Option<OrgId>, Package, Revision) {
        (self.fetcher, self.org_id, self.package, self.revision)
    }
}

impl Display for StrictLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fetcher = &self.fetcher;
        write!(f, "{fetcher}+")?;

        if let Some(org_id) = &self.org_id {
            write!(f, "{org_id}/")?;
        }

        let package = &self.package;
        let revision = &self.revision;
        write!(f, "{package}${revision}")?;

        Ok(())
    }
}

impl<'de> Deserialize<'de> for StrictLocator {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        StrictLocator::parse(&raw).map_err(serde::de::Error::custom)
    }
}

impl Serialize for StrictLocator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

impl<'a> ToSchema<'a> for StrictLocator {
    fn schema() -> (
        &'a str,
        utoipa::openapi::RefOr<utoipa::openapi::schema::Schema>,
    ) {
        (
            "StrictLocator",
            ObjectBuilder::new()
                .description(Some(Self::DOCS))
                .example(Some(json!("git+github.com/fossas/example$1234")))
                .min_length(Some(3))
                .schema_type(SchemaType::String)
                .build()
                .into(),
        )
    }
}

impl AsRef<StrictLocator> for StrictLocator {
    fn as_ref(&self) -> &StrictLocator {
        self
    }
}

impl FromStr for StrictLocator {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s)
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use impls::impls;
    use itertools::{izip, Itertools};
    use pretty_assertions::assert_eq;
    use serde::Deserialize;
    use static_assertions::const_assert;
    use strum::IntoEnumIterator;

    use super::*;

    #[test]
    fn trait_impls() {
        const_assert!(impls!(StrictLocator: AsRef<StrictLocator>));
        const_assert!(impls!(StrictLocator: FromStr));
    }

    #[test]
    fn parse_using_fromstr() {
        let input = "git+github.com/foo/bar$abcd";
        let parsed = input.parse().expect("must parse locator");
        let expected = strict!(Git, "github.com/foo/bar", "abcd");
        assert_eq!(expected, parsed);
        assert_eq!(&parsed.to_string(), input);
    }

    #[test]
    fn parse_render_successful() {
        let input = "git+github.com/foo/bar$abcd";
        let parsed = StrictLocator::parse(input).expect("must parse locator");
        let expected = StrictLocator::builder()
            .fetcher(Fetcher::Git)
            .package("github.com/foo/bar")
            .revision("abcd")
            .build();
        assert_eq!(expected, parsed);
        assert_eq!(&parsed.to_string(), input);
    }

    #[test]
    fn parse_invalid_fetcher() {
        let input = "foo+github.com/foo/bar";
        let parsed = StrictLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Fetcher { .. })));
    }

    #[test]
    fn parse_missing_package() {
        let input = "git+";
        let parsed = StrictLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
    }

    #[test]
    fn parse_missing_revision() {
        let input = "git+package";
        let parsed = StrictLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
    }

    #[test]
    fn parse_empty_revision() {
        let input = "git+package$";
        let parsed = StrictLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
    }

    #[test]
    fn parse_invalid_syntax() {
        let input = "";
        let parsed = StrictLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Syntax { .. })));

        let input = "git+$";
        let parsed = StrictLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
    }

    #[test]
    fn parse_with_org() {
        let fetchers = Fetcher::iter().map(|fetcher| format!("{fetcher}"));
        let orgs = [
            OrgId(0usize),
            OrgId(1),
            OrgId(1234),
            OrgId(2385028),
            OrgId(19847938492847928),
        ];
        let packages = ["github.com/foo/bar", "some-name"];
        let revisions = ["1", "1234abcd1234"];

        for (fetcher, org, package, revision) in izip!(fetchers, orgs, packages, revisions) {
            let input = format!("{fetcher}+{org}/{package}${revision}");
            let Ok(parsed) = StrictLocator::parse(&input) else {
                panic!("must parse '{input}'")
            };

            assert_eq!(
                parsed.fetcher().to_string(),
                fetcher,
                "'fetcher' in '{input}' must match"
            );
            assert_eq!(
                parsed.org_id(),
                Some(org),
                "'org_id' in '{input}' must match"
            );
            assert_eq!(
                parsed.package().as_str(),
                package,
                "'package' in '{input}' must match"
            );
            assert_eq!(
                parsed.revision().as_str(),
                revision,
                "'revision' in '{input}' must match",
            );
        }
    }

    #[test]
    fn render_with_org() {
        let locator = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .org_id(1234)
            .package("foo/bar")
            .revision("123abc")
            .build();

        let rendered = locator.to_string();
        assert_eq!("custom+1234/foo/bar$123abc", rendered);
    }

    #[test]
    fn render_with_revision() {
        let locator = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo/bar")
            .revision("123abc")
            .build();

        let rendered = locator.to_string();
        assert_eq!("custom+foo/bar$123abc", rendered);
    }

    #[test]
    fn roundtrip_serialization() {
        let input = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo")
            .revision("bar")
            .org_id(1)
            .build();

        let serialized = serde_json::to_string(&input).expect("must serialize");
        let deserialized = serde_json::from_str(&serialized).expect("must deserialize");
        assert_eq!(input, deserialized);
    }

    #[test]
    fn serde_deserialization() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Test {
            locator: StrictLocator,
        }

        let input = r#"{ "locator": "custom+1/foo$bar" }"#;
        let locator = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo")
            .revision("bar")
            .org_id(1)
            .build();

        let expected = Test { locator };
        let deserialized = serde_json::from_str(input).expect("must deserialize");
        assert_eq!(expected, deserialized, "deserialize {input}");
    }

    #[test]
    fn demotes_package() {
        let input = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo")
            .revision("bar")
            .org_id(1)
            .build();

        let expected = PackageLocator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo")
            .org_id(1)
            .build();
        let demoted = input.clone().into_package();
        assert_eq!(expected, demoted, "demote {input}");
    }

    #[test]
    fn demotes_locator() {
        let input = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo")
            .revision("bar")
            .org_id(1)
            .build();

        let expected = Locator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo")
            .org_id(1)
            .revision("bar")
            .build();
        let demoted = input.clone().into_locator();
        assert_eq!(expected, demoted, "demote {input}");
    }

    #[test]
    fn ordering() {
        let locators = vec![
            "git+github.com/foo/bar$abcd10",
            "git+github.com/foo/bar$abcd11",
            "custom+baz$1234",
            "custom+1/bam$1234",
            "custom+2/bam$1234",
        ]
        .into_iter()
        .map(StrictLocator::parse)
        .collect::<Result<Vec<_>, _>>()
        .expect("must parse locators");

        let expected = vec![
            "custom+baz$1234",
            "custom+1/bam$1234",
            "custom+2/bam$1234",
            "git+github.com/foo/bar$abcd10",
            "git+github.com/foo/bar$abcd11",
        ];
        let sorted = locators
            .iter()
            .sorted()
            .map(StrictLocator::to_string)
            .collect_vec();
        assert_eq!(expected, sorted, "sort {locators:?}");
    }
}
