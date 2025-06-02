use derive_more::{Display, FromStr};
use documented::Documented;
use locator_codegen::locator_parts;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

use crate::{self as locator, Ecosystem, Empty, Locator, OrgId, Package, StrictLocator};

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
            .organization($org)
            .revision($crate::Empty)
            .build()
    };
    ($ecosystem:ident, $package:expr) => {
        $crate::PackageLocator::builder()
            .ecosystem($crate::ecosystems::Ecosystem::$ecosystem)
            .package($package)
            .revision($crate::Empty)
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
#[locator_parts(
    types(Ecosystem, Option<OrgId>, Package, Empty),
    get(ecosystem = pub fn ecosystem(&self) -> Ecosystem { self.0.ecosystem }),
    get(organization = pub fn organization(&self) -> Option<OrgId> { self.0.organization }),
)]
#[derive(
    Clone,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Debug,
    Documented,
    Serialize,
    Deserialize,
    Display,
    ToSchema,
    FromStr,
)]
#[schema(
    value_type = String,
    example = json!("npm+lodash"),
    example = json!("npm+1234/lodash"),
    example = json!("git+github.com/fossas/locator-rs"),
)]
pub struct PackageLocator;

impl From<Locator> for PackageLocator {
    fn from(value: Locator) -> Self {
        value.into_parts().map_revision(|_| Empty).into()
    }
}

impl From<StrictLocator> for PackageLocator {
    fn from(value: StrictLocator) -> Self {
        value.into_parts().map_revision(|_| Empty).into()
    }
}

impl<T: Into<PackageLocator> + Clone> From<&T> for PackageLocator {
    fn from(value: &T) -> Self {
        value.clone().into()
    }
}

impl AsRef<PackageLocator> for PackageLocator {
    fn as_ref(&self) -> &PackageLocator {
        self
    }
}
