use derive_more::{Display, FromStr};
use documented::Documented;
use locator_codegen::locator_parts;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

use crate::{self as locator, Ecosystem, OrgId, Package, PackageLocator, Revision, StrictLocator};

/// Convenience macro for creating a [`Locator`].
/// Required types and fields are checked at compile time.
///
/// ```
/// let loc = locator::locator!(Npm, "lodash");
/// assert_eq!("npm+lodash", &loc.to_string());
///
/// let loc = locator::locator!(Npm, "lodash", "1.0.0");
/// assert_eq!("npm+lodash$1.0.0", &loc.to_string());
///
/// let loc = locator::locator!(org 1234 => Npm, "lodash");
/// assert_eq!("npm+1234/lodash", &loc.to_string());
///
/// let loc = locator::locator!(org 1234 => Npm, "lodash", "1.0.0");
/// assert_eq!("npm+1234/lodash$1.0.0", &loc.to_string());
/// ```
#[macro_export]
macro_rules! locator {
    (org $org:expr => $ecosystem:ident, $package:expr, $version:expr) => {
        $crate::Locator::builder()
            .ecosystem($crate::ecosystems::Ecosystem::$ecosystem)
            .package($package)
            .organization($org)
            .revision($crate::revision!($version))
            .build()
    };
    (org $org:expr => $ecosystem:ident, $package:expr) => {
        $crate::Locator::builder()
            .ecosystem($crate::ecosystems::Ecosystem::$ecosystem)
            .package($package)
            .organization($org)
            .build()
    };
    ($ecosystem:ident, $package:expr, $version:expr) => {
        $crate::Locator::builder()
            .ecosystem($crate::ecosystems::Ecosystem::$ecosystem)
            .package($package)
            .revision($crate::revision!($version))
            .build()
    };
    ($ecosystem:ident, $package:expr) => {
        $crate::Locator::builder()
            .ecosystem($crate::ecosystems::Ecosystem::$ecosystem)
            .package($package)
            .build()
    };
}

/// `Locator` identifies a package, optionally at a specific revision, in a code host.
///
/// If the `revision` component is not specified, FOSSA services interpret this to mean
/// that the "latest" version of the package should be used if the requested operation
/// requires a concrete version of the package.
///
/// ## Guarantees
///
/// This type represents a _validly-constructed_ `Locator`, but does not
/// guarantee whether a package or revision actually exists or is accessible
/// in the code host.
///
/// ## Ordering
///
/// `Locator` orders by:
/// 1. ecosystem, alphanumerically.
/// 2. Organization ID, alphanumerically; missing organizations are sorted higher.
/// 3. The package field, alphanumerically.
/// 4. The revision field:
///   - If both comparing locators use semver, these are compared using semver rules.
///   - Otherwise these are compared alphanumerically.
///   - Missing revisions are sorted higher.
///
/// **Important:** there may be other metrics for ordering using the actual code host
/// which contains the package- for example ordering by release date, or code hosts
/// such as `git` which have non-linear history (making flat ordering a lossy operation).
/// `Locator` does not take such edge cases into account in any way.
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
    types(Ecosystem, Option<OrgId>, Package, Option<Revision>),
    get(ecosystem = pub fn ecosystem(&self) -> Ecosystem { self.0.ecosystem }),
    get(organization = pub fn organization(&self) -> Option<OrgId> { self.0.organization }),
    get(revision = pub fn revision(&self) -> Option<&Revision> { self.0.revision.as_ref() }),
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
    example = json!("npm+lodash$1.0.0"),
    example = json!("npm+1234/lodash$1.0.0"),
    example = json!("git+github.com/fossas/locator-rs$v4.0.0"),
)]
pub struct Locator;

impl From<PackageLocator> for Locator {
    fn from(value: PackageLocator) -> Self {
        value.into_parts().map_revision(|_| None).into()
    }
}

impl From<StrictLocator> for Locator {
    fn from(value: StrictLocator) -> Self {
        value.into_parts().map_revision(Some).into()
    }
}

impl AsRef<Locator> for Locator {
    fn as_ref(&self) -> &Locator {
        self
    }
}

impl<T: Into<Locator> + Clone> From<&T> for Locator {
    fn from(value: &T) -> Self {
        value.clone().into()
    }
}

#[cfg(test)]
mod tests {
    use crate::revision;

    use super::*;

    #[test]
    fn conv() {
        let a = Locator::builder()
            .ecosystem(Ecosystem::Npm)
            .package("foo")
            .revision(revision!("bar"))
            .build();
        let rev = a.revision();
        assert_eq!(rev, Some(&revision!("bar")));
    }
}
