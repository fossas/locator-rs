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

use crate::{Error, Locator, OrgId, Package, StrictLocator, ecosystems::Ecosystem};

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
    (org $org:expr => $ecosystem:ident, $package:expr) => {
        $crate::PackageLocator::builder()
            .ecosystem($crate::ecosystems::Ecosystem::$ecosystem)
            .package($package)
            .org_id($org)
            .build()
    };
    ($ecosystem:ident, $package:expr) => {
        $crate::PackageLocator::builder()
            .ecosystem($crate::ecosystems::Ecosystem::$ecosystem)
            .package($package)
            .build()
    };
}

/// `PackageLocator` identifies a package in a code host.
///
/// "Package" locators are similar to standard locators, except that they
/// _never specify_ the `revision` field. If the `revision` field
/// is provided in the input string, `PackageLocator` ignores it.
///
/// ## Guarantees
///
/// This type represents a _validly-constructed_ `PackageLocator`, but does not
/// guarantee whether a package actually exists or is accessible in the code host.
///
/// ## Ordering
///
/// `PackageLocator` orders by:
/// 1. Ecosystem, alphanumerically.
/// 2. Organization ID, alphanumerically; missing organizations are sorted higher.
/// 3. The package field, alphanumerically.
///
/// **Important:** there may be other metrics for ordering using the actual code host
/// which contains the package- for example ordering by release date.
/// `PackageLocator` does not take such edge cases into account in any way.
///
/// ## Parsing
///
/// This type is canonically rendered to a string before being serialized
/// to the database or sent over the network according to the rules in this section.
///
/// The input string must be in one of the following formats:
/// ```ignore
/// {ecosystem}+{package}${revision}
/// {ecosystem}+{package}
/// ```
///
/// Packages may also be namespaced to a specific organization;
/// in such cases the organization ID is at the start of the `{package}` field
/// separated by a slash. The ID can be any non-negative integer.
/// This yields the following optional formats:
/// ```ignore
/// {ecosystem}+{org_id}/{package}${revision}
/// {ecosystem}+{org_id}/{package}
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
pub struct PackageLocator {
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
            .ecosystem(self.ecosystem)
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
            .ecosystem(self.ecosystem)
            .package(self.package)
            .revision(revision.to_string());

        match self.org_id {
            None => locator.build(),
            Some(OrgId(id)) => locator.org_id(id).build(),
        }
    }

    /// Explodes the locator into its (owned) parts.
    /// Used for conversions without cloning.
    pub(crate) fn explode(self) -> (Ecosystem, Option<OrgId>, Package) {
        (self.ecosystem, self.org_id, self.package)
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
        let (ecosystem, org_id, package, _) = full.explode();
        Self {
            ecosystem,
            org_id,
            package,
        }
    }
}

impl From<&Locator> for PackageLocator {
    fn from(full: &Locator) -> Self {
        Self {
            ecosystem: full.ecosystem(),
            org_id: full.org_id(),
            package: full.package().clone(),
        }
    }
}

impl From<StrictLocator> for PackageLocator {
    fn from(strict: StrictLocator) -> Self {
        let (ecosystem, org_id, package, _) = strict.explode();
        Self {
            ecosystem,
            org_id,
            package,
        }
    }
}

impl From<&StrictLocator> for PackageLocator {
    fn from(strict: &StrictLocator) -> Self {
        Self {
            ecosystem: strict.ecosystem(),
            org_id: strict.org_id(),
            package: strict.package().clone(),
        }
    }
}

impl From<&PackageLocator> for PackageLocator {
    fn from(locator: &PackageLocator) -> Self {
        locator.clone()
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

impl PartialSchema for PackageLocator {
    fn schema() -> utoipa::openapi::RefOr<utoipa::openapi::schema::Schema> {
        ObjectBuilder::new()
            .description(Some(Self::DOCS))
            .examples([json!("git+github.com/fossas/locator-rs")])
            .min_length(Some(3))
            .schema_type(Type::String)
            .build()
            .into()
    }
}

impl ToSchema for PackageLocator {
    fn name() -> Cow<'static, str> {
        Cow::Borrowed("PackageLocator")
    }
}
