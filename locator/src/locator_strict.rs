use derive_more::{Display, FromStr};
use documented::Documented;
use locator_codegen::locator_parts;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

use crate::{self as locator, Ecosystem, Locator, OrgId, Package, Revision};

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
            .organization($org)
            .revision($crate::revision!($version))
            .build()
    };
    ($ecosystem:ident, $package:expr, $version:expr) => {
        $crate::StrictLocator::builder()
            .ecosystem($crate::ecosystems::Ecosystem::$ecosystem)
            .package($package)
            .revision($crate::revision!($version))
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
#[locator_parts(
    types(Ecosystem, Option<OrgId>, Package, Revision),
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
    example = json!("npm+lodash$1.0.0"),
    example = json!("npm+1234/lodash$1.0.0"),
    example = json!("git+github.com/fossas/locator-rs$v4.0.0"),
    example = json!("npm+lodash"),
    example = json!("npm+1234/lodash"),
    example = json!("git+github.com/fossas/locator-rs"),
)]
pub struct StrictLocator;

impl TryFrom<Locator> for StrictLocator {
    type Error = Locator;

    fn try_from(value: Locator) -> Result<Self, Self::Error> {
        value
            .into_parts()
            .try_map_revision(|r| match r {
                Some(r) => Ok(r),
                None => Err(r),
            })
            .map(Self)
            .map_err(|t| Locator::from_parts(t.into()))
    }
}

impl TryFrom<&Locator> for StrictLocator {
    type Error = Locator;

    fn try_from(value: &Locator) -> Result<Self, Self::Error> {
        value
            .clone()
            .into_parts()
            .try_map_revision(|r| match r {
                Some(r) => Ok(r),
                None => Err(r),
            })
            .map(Self)
            .map_err(|t| Locator::from_parts(t.into()))
    }
}

impl<T: Into<StrictLocator> + Clone> From<&T> for StrictLocator {
    fn from(value: &T) -> Self {
        value.clone().into()
    }
}

impl AsRef<StrictLocator> for StrictLocator {
    fn as_ref(&self) -> &StrictLocator {
        self
    }
}

#[cfg(test)]
mod tests {
    use crate::revision;

    use super::*;

    #[test]
    fn conv() {
        let a = StrictLocator::builder()
            .ecosystem(Ecosystem::Npm)
            .package("foo")
            .revision(revision!("bar"))
            .build();
        let org = a.organization();
        assert_eq!(org, None);
    }
}
