use std::{fmt::Display, str::FromStr};

use bon::Builder;
use documented::Documented;
use getset::{CopyGetters, Getters};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::borrow::Cow;
use utoipa::{
    PartialSchema, ToSchema,
    openapi::{ObjectBuilder, Type},
};

use crate::{
    Error, Locator, OrgId, Package, PackageLocator, ParseError, Revision, ecosystems::Ecosystem,
};

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
    (org $org:expr => $ecosystem:ident, $package:expr, $version:expr) => {
        $crate::StrictLocator::builder()
            .ecosystem($crate::ecosystems::Ecosystem::$ecosystem)
            .package($package)
            .org_id($org)
            .revision($version)
            .build()
    };
    ($ecosystem:ident, $package:expr, $version:expr) => {
        $crate::StrictLocator::builder()
            .ecosystem($crate::ecosystems::Ecosystem::$ecosystem)
            .package($package)
            .revision($version)
            .build()
    };
}

/// `StrictLocator` identifies a package at a specific revision in a code host.
///
/// "Strict" locators are similar to standard locators, except that they
/// _require_ the `revision` field to be specified. If the `revision` field
/// is not specified, `StrictLocator` fails to parse.
///
/// ## Guarantees
///
/// This type represents a _validly-constructed_ `StrictLocator`, but does not
/// guarantee whether a package or revision actually exists or is accessible
/// in the code host.
///
/// ## Ordering
///
/// `StrictLocator` orders by:
/// 1. Fetcher, alphanumerically.
/// 2. Organization ID, alphanumerically; missing organizations are sorted higher.
/// 3. The package field, alphanumerically.
/// 4. The revision field:
///   - If both comparing locators use semver, these are compared using semver rules.
///   - Otherwise these are compared alphanumerically.
///
/// **Important:** there may be other metrics for ordering using the actual code host
/// which contains the package- for example ordering by release date, or code hosts
/// such as `git` which have non-linear history (making flat ordering a lossy operation).
/// `StrictLocator` does not take such edge cases into account in any way.
///
/// ## Parsing
///
/// This type is canonically rendered to a string before being serialized
/// to the database or sent over the network according to the rules in this section.
///
/// The input string must be in the following format:
/// ```ignore
/// {ecosystem}+{package}${revision}
/// ```
///
/// Packages may also be namespaced to a specific organization;
/// in such cases the organization ID is at the start of the `{package}` field
/// separated by a slash. The ID can be any non-negative integer.
/// This yields the following optional format:
/// ```ignore
/// {ecosystem}+{org_id}/{package}${revision}
/// ```
///
/// Note that locators do not feature escaping: instead the _first_ instance
/// of each delimiter (`+`, `/`, `$`) is used to split the fields. However,
/// as a special case organization IDs are only extracted if the field content
/// fully consists of a non-negative integer.
//
// For more information on the background of `Locator` and ecosystems generally,
// FOSSA employees may refer to the "ecosystems and locators" doc: https://go/ecosystems-doc.
#[derive(
    Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Builder, Getters, CopyGetters, Documented,
)]
pub struct StrictLocator {
    /// Determines which ecosystem is used to download this package.
    #[getset(get_copy = "pub")]
    ecosystem: Ecosystem,

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
    #[builder(into)]
    #[getset(get_copy = "pub")]
    org_id: Option<OrgId>,

    /// Specifies the unique identifier for the package by ecosystem.
    ///
    /// For example, the `git` ecosystem fetching a github package
    /// uses a value in the form of `{user_name}/{package_name}`.
    #[builder(into)]
    #[getset(get = "pub")]
    package: Package,

    /// Specifies the version for the package by ecosystem.
    ///
    /// For example, the `git` ecosystem fetching a github package
    /// uses a value in the form of `{git_sha}` or `{git_tag}`,
    /// and the ecosystem disambiguates.
    #[builder(into)]
    #[getset(get = "pub")]
    revision: Revision,
}

impl StrictLocator {
    /// Parse a `StrictLocator`.
    /// For details, see the parsing section on [`StrictLocator`].
    pub fn parse(locator: &str) -> Result<Self, Error> {
        let (ecosystem, org_id, package, revision) = Locator::parse(locator)?.explode();

        let Some(revision) = revision else {
            return Err(Error::Parse(ParseError::Field {
                input: locator.to_owned(),
                field: String::from("revision"),
            }));
        };

        Ok(Self {
            ecosystem,
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
    pub(crate) fn explode(self) -> (Ecosystem, Option<OrgId>, Package, Revision) {
        (self.ecosystem, self.org_id, self.package, self.revision)
    }
}

impl Display for StrictLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ecosystem = &self.ecosystem;
        write!(f, "{ecosystem}+")?;

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

impl From<&StrictLocator> for StrictLocator {
    fn from(locator: &StrictLocator) -> Self {
        locator.clone()
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

impl PartialSchema for StrictLocator {
    fn schema() -> utoipa::openapi::RefOr<utoipa::openapi::schema::Schema> {
        ObjectBuilder::new()
            .description(Some(Self::DOCS))
            .examples([json!("git+github.com/fossas/locator-rs$v1.0.0")])
            .min_length(Some(3))
            .schema_type(Type::String)
            .build()
            .into()
    }
}

impl ToSchema for StrictLocator {
    fn name() -> Cow<'static, str> {
        Cow::Borrowed("StrictLocator")
    }
}
