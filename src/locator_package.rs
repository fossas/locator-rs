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

use crate::{Error, Fetcher, Locator, OrgId, Package, StrictLocator};

/// Convenience macro for creating a [`PackageLocator`].
/// Required types and fields are checked at compile time.
///
/// ```
/// let loc = locator::package!(Npm, "lodash");
/// assert_eq!("npm+lodash", &loc.to_string());
///
/// let loc = locator::package!(org 1234 => Npm, "lodash");
/// assert_eq!("npm+1234/lodash", &loc.to_string());
/// ```
#[macro_export]
macro_rules! package {
    (org $org:expr => $fetcher:ident, $package:expr) => {
        $crate::PackageLocator::builder()
            .fetcher($crate::Fetcher::$fetcher)
            .package($package)
            .org_id($org)
            .build()
    };
    ($fetcher:ident, $package:expr) => {
        $crate::PackageLocator::builder()
            .fetcher($crate::Fetcher::$fetcher)
            .package($package)
            .build()
    };
}

/// A [`Locator`] specialized to not include the `revision` component.
///
/// Any [`Locator`] may be converted to a `PackageLocator` by simply discarding the `revision` component.
/// To create a [`Locator`] from a `PackageLocator`, the value for `revision` must be provided; see [`Locator`] for details.
///
/// ## Ordering
///
/// Locators order by:
/// 1. Fetcher, alphanumerically.
/// 2. Organization ID, alphanumerically; missing organizations are sorted higher.
/// 3. The package field, alphanumerically.
///
/// Importantly, there may be other metrics for ordering using the actual code host
/// which contains the package (for example, ordering by release date).
/// This library does not perform such ordering.
///
/// ## Parsing
///
/// The input string must be in one of the following forms:
/// - `{fetcher}+{package}`
/// - `{fetcher}+{package}$`
/// - `{fetcher}+{package}${revision}`
///
/// Packages may also be namespaced to a specific organization;
/// in such cases the organization ID is at the start of the `{package}` field
/// separated by a slash. The ID can be any non-negative integer.
/// This yields the following formats:
/// - `{fetcher}+{org_id}/{package}`
/// - `{fetcher}+{org_id}/{package}$`
/// - `{fetcher}+{org_id}/{package}${revision}`
///
/// This implementation ignores the `revision` segment if it exists. If this is not preferred, use [`Locator`] instead.
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
pub struct PackageLocator {
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
}

impl PackageLocator {
    /// Parse a `PackageLocator`.
    /// For details, see the parsing section on [`PackageLocator`].
    pub fn parse(locator: &str) -> Result<Self, Error> {
        let full = Locator::parse(locator)?;
        Ok(full.into_package())
    }

    /// Promote a `PackageLocator` to a [`Locator`] by providing the value to use for the `revision` component.
    pub fn promote(self, revision: Option<String>) -> Locator {
        let locator = Locator::builder()
            .fetcher(self.fetcher)
            .package(self.package);

        match (self.org_id, revision) {
            (None, None) => locator.build(),
            (None, Some(revision)) => locator.revision(revision).build(),
            (Some(OrgId(id)), None) => locator.org_id(id).build(),
            (Some(OrgId(id)), Some(revision)) => locator.org_id(id).revision(revision).build(),
        }
    }

    /// Promote a `PackageLocator` to a [`StrictLocator`] by providing the value to use for the `revision` component.
    pub fn promote_strict(self, revision: impl ToString) -> StrictLocator {
        let locator = StrictLocator::builder()
            .fetcher(self.fetcher)
            .package(self.package)
            .revision(revision);

        match self.org_id {
            None => locator.build(),
            Some(OrgId(id)) => locator.org_id(id).build(),
        }
    }

    /// Explodes the locator into its (owned) parts.
    /// Used for conversions without cloning.
    pub(crate) fn explode(self) -> (Fetcher, Option<OrgId>, Package) {
        (self.fetcher, self.org_id, self.package)
    }
}

impl Display for PackageLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let converted = Locator::from(self);
        write!(f, "{converted}")
    }
}

impl<'de> Deserialize<'de> for PackageLocator {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        PackageLocator::parse(&raw).map_err(serde::de::Error::custom)
    }
}

impl<'a> ToSchema<'a> for PackageLocator {
    fn schema() -> (
        &'a str,
        utoipa::openapi::RefOr<utoipa::openapi::schema::Schema>,
    ) {
        (
            "PackageLocator",
            ObjectBuilder::new()
                .description(Some(Self::DOCS))
                .example(Some(json!("git+github.com/fossas/example")))
                .min_length(Some(3))
                .schema_type(SchemaType::String)
                .build()
                .into(),
        )
    }
}

impl Serialize for PackageLocator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

impl From<Locator> for PackageLocator {
    fn from(full: Locator) -> Self {
        let (fetcher, org_id, package, _) = full.explode();
        Self {
            fetcher,
            org_id,
            package,
        }
    }
}

impl From<&Locator> for PackageLocator {
    fn from(full: &Locator) -> Self {
        Self {
            fetcher: full.fetcher(),
            org_id: full.org_id(),
            package: full.package().clone(),
        }
    }
}

impl From<StrictLocator> for PackageLocator {
    fn from(strict: StrictLocator) -> Self {
        let (fetcher, org_id, package, _) = strict.explode();
        Self {
            fetcher,
            org_id,
            package,
        }
    }
}

impl From<&StrictLocator> for PackageLocator {
    fn from(strict: &StrictLocator) -> Self {
        Self {
            fetcher: strict.fetcher(),
            org_id: strict.org_id(),
            package: strict.package().clone(),
        }
    }
}

impl AsRef<PackageLocator> for PackageLocator {
    fn as_ref(&self) -> &PackageLocator {
        self
    }
}

impl FromStr for PackageLocator {
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

    use crate::ParseError;

    use super::*;

    #[test]
    fn trait_impls() {
        const_assert!(impls!(PackageLocator: AsRef<PackageLocator>));
        const_assert!(impls!(PackageLocator: FromStr));
        const_assert!(impls!(PackageLocator: From<StrictLocator>));
        const_assert!(impls!(PackageLocator: From<Locator>));
    }

    #[test]
    fn parse_using_fromstr() {
        let input = "git+github.com/foo/bar";
        let parsed = input.parse().expect("must parse locator");
        let expected = package!(Git, "github.com/foo/bar");
        assert_eq!(expected, parsed);
        assert_eq!(&parsed.to_string(), input);
    }

    #[test]
    fn parse_render_successful() {
        let input = "git+github.com/foo/bar";
        let parsed = PackageLocator::parse(input).expect("must parse locator");
        let expected = PackageLocator::builder()
            .fetcher(Fetcher::Git)
            .package("github.com/foo/bar")
            .build();
        assert_eq!(expected, parsed);
        assert_eq!(&parsed.to_string(), input);
    }

    #[test]
    fn parse_drops_revision() {
        let input = "git+github.com/foo/bar$abcd";
        let parsed = PackageLocator::parse(input).expect("must parse locator");
        let expected = PackageLocator::builder()
            .fetcher(Fetcher::Git)
            .package("github.com/foo/bar")
            .build();
        assert_eq!(expected, parsed);
    }

    #[test]
    fn parse_invalid_fetcher() {
        let input = "foo+github.com/foo/bar";
        let parsed = PackageLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Fetcher { .. })));
    }

    #[test]
    fn parse_missing_package() {
        let input = "git+";
        let parsed = PackageLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
    }

    #[test]
    fn parse_invalid_syntax() {
        let input = "";
        let parsed = PackageLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Syntax { .. })));

        let input = "git+$";
        let parsed = PackageLocator::parse(input);
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
        let revisions = ["", "$", "$1", "$1234abcd1234"];

        for (fetcher, org, package, revision) in izip!(fetchers, orgs, packages, revisions) {
            let input = format!("{fetcher}+{org}/{package}{revision}");
            let Ok(parsed) = PackageLocator::parse(&input) else {
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
        }
    }

    #[test]
    fn render_with_org() {
        let locator = PackageLocator::builder()
            .fetcher(Fetcher::Custom)
            .org_id(1234)
            .package("foo/bar")
            .build();

        let rendered = locator.to_string();
        assert_eq!("custom+1234/foo/bar", rendered);
    }

    #[test]
    fn roundtrip_serialization() {
        let input = PackageLocator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo")
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
            locator: PackageLocator,
        }

        let input = r#"{ "locator": "custom+1/foo" }"#;
        let locator = PackageLocator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo")
            .org_id(1)
            .build();

        let expected = Test { locator };
        let deserialized = serde_json::from_str(input).expect("must deserialize");
        assert_eq!(expected, deserialized, "deserialize {input}");
    }

    #[test]
    fn promotes_locator() {
        let input = PackageLocator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo")
            .org_id(1)
            .build();

        let expected = Locator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo")
            .org_id(1)
            .build();
        let promoted = input.clone().promote(None);
        assert_eq!(expected, promoted, "promote {input}");

        let expected = Locator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo")
            .org_id(1)
            .revision("bar")
            .build();
        let promoted = input.clone().promote(Some(String::from("bar")));
        assert_eq!(expected, promoted, "promote {input}");
    }

    #[test]
    fn promotes_strict() {
        let input = PackageLocator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo")
            .org_id(1)
            .build();

        let expected = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .package("foo")
            .org_id(1)
            .revision("bar")
            .build();
        let promoted = input.clone().promote_strict("bar");
        assert_eq!(expected, promoted, "promote {input}");
    }

    #[test]
    fn ordering() {
        let locators = vec![
            "git+github.com/foo/bar",
            "git+github.com/foo/bar$1234",
            "custom+baz$1234",
            "custom+1/bam$1234",
            "custom+2/bam$1234",
        ]
        .into_iter()
        .map(PackageLocator::parse)
        .collect::<Result<Vec<_>, _>>()
        .expect("must parse locators");

        let expected = vec![
            "custom+baz",
            "custom+1/bam",
            "custom+2/bam",
            "git+github.com/foo/bar",
            "git+github.com/foo/bar",
        ];
        let sorted = locators
            .iter()
            .sorted()
            .map(PackageLocator::to_string)
            .collect_vec();
        assert_eq!(expected, sorted, "sort {locators:?}");
    }
}
