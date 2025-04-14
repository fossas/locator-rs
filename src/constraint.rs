use derive_new::new;
use documented::Documented;
use enum_assoc::Assoc;
use serde::{Deserialize, Serialize};
use strum::Display;
use utoipa::ToSchema;

use crate::{Fetcher, Revision};

mod fallback;

/// Describes version constraints supported by this crate.
///
/// Note that different fetchers may interpret these constraints in different ways-
/// for example `compatible` isn't the same in Cargo as it is in Cabal.
///
/// # Serialization
///
/// The default serialization for this type is for transporting _this type_;
/// it is not meant to support parsing the actual constraints in the native format
/// used by the package manager.
///
/// # Comparison
///
/// Compares the [`Constraint`] to the given [`Revision`] according to the rules of the provided [`Fetcher`].
///
/// If there are no rules for that specific fetcher, the following fallbacks take place:
/// - If both the `Constraint` and the `Revision` are parseable as semver, compare according to semver rules.
/// - Otherwise, they are coerced to an opaque string and compared according to unicode ordering rules.
///
/// When comparing according to unicode:
/// - `Equal` and `NotEqual` compare bytewise.
/// - `Compatible` compares case insensitively, folding non-ASCII characters into their closest ASCII equivalent prior to comparing.
/// - All other variants compare lexically, folding non-ASCII characters into their closest ASCII equivalent prior to comparing.
///
/// In practice this means that `Equal` and `NotEqual` are case-sensitive, while all other options are not;
/// this case-insensitivity does its best to preserve the spirit of this intent in the face of non-ascii inputs.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deserialize,
    Serialize,
    Documented,
    ToSchema,
    Display,
    Assoc,
    new,
)]
#[schema(example = json!({ "kind": "equal", "value": "1.0.0"}))]
#[serde(rename_all = "snake_case", tag = "kind", content = "value")]
#[func(const fn revision(&self) -> &Revision)]
#[new(into)]
#[non_exhaustive]
pub enum Constraint {
    /// The comparing revision must be compatible with the provided revision.
    /// Note that the exact semantics of this constraint depend on the fetcher.
    #[strum(serialize = "~={0:?}")]
    #[assoc(revision = &_0)]
    Compatible(Revision),

    /// The comparing revision must be exactly equal to the provided revision.
    #[strum(serialize = "=={0:?}")]
    #[assoc(revision = &_0)]
    Equal(Revision),

    /// The comparing revision must not be exactly equal to the provided revision.
    #[strum(serialize = "!={0:?}")]
    #[assoc(revision = &_0)]
    NotEqual(Revision),

    /// The comparing revision must be less than the provided revision.
    #[strum(serialize = "<{0:?}")]
    #[assoc(revision = &_0)]
    Less(Revision),

    /// The comparing revision must be less than or equal to the provided revision.
    #[strum(serialize = "<={0:?}")]
    #[assoc(revision = &_0)]
    LessOrEqual(Revision),

    /// The comparing revision must be greater than the provided revision.
    #[strum(serialize = ">{0:?}")]
    #[assoc(revision = &_0)]
    Greater(Revision),

    /// The comparing revision must be greater than or equal to the provided revision.
    #[strum(serialize = ">={0:?}")]
    #[assoc(revision = &_0)]
    GreaterOrEqual(Revision),
}

impl Constraint {
    /// Compare the constraint to the target revision according to the rules of the provided fetcher.
    ///
    /// The default if there are no additional rules specified for the fetcher is:
    /// - If both versions are semver, compare according to semver rules.
    /// - If not, coerce them both to an opaque string and compare according to unicode ordering rules.
    ///   In this instance [`Constraint::Compatible`] is a case-insensitive equality comparison.
    pub fn compare(&self, fetcher: Fetcher, target: &Revision) -> bool {
        match fetcher {
            // If no specific comparitor is configured for this fetcher,
            // compare using the generic fallback.
            other => fallback::compare(self, other, target),
        }
    }
}

impl From<&Constraint> for Constraint {
    fn from(c: &Constraint) -> Self {
        c.clone()
    }
}

/// A set of [`Constraint`].
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, Documented, ToSchema)]
#[non_exhaustive]
pub struct Constraints(Vec<Constraint>);

impl Constraints {
    /// Iterate over constraints in the set.
    pub fn iter(&self) -> impl Iterator<Item = &Constraint> {
        self.0.iter()
    }

    /// Compare the constraints to the target revision according to the rules of the provided fetcher.
    ///
    /// This method compares in an `AND` fashion: it only returns true if _all_ constraints
    /// inside of this set compare favorably.
    pub fn compare_all(&self, fetcher: Fetcher, target: &Revision) -> bool {
        for constraint in self.iter() {
            if !constraint.compare(fetcher, target) {
                return false;
            }
        }
        true
    }

    /// Compare the constraints to the target revision according to the rules of the provided fetcher.
    ///
    /// This method compares in an `OR` fashion: it only returns true if _any_ constraint
    /// inside of this set compare favorably.
    pub fn compare_any(&self, fetcher: Fetcher, target: &Revision) -> bool {
        for constraint in self.iter() {
            if constraint.compare(fetcher, target) {
                return true;
            }
        }
        false
    }
}

impl<I, T> From<I> for Constraints
where
    I: IntoIterator<Item = T>,
    T: Into<Constraint>,
{
    fn from(constraints: I) -> Self {
        Self(constraints.into_iter().map(Into::into).collect())
    }
}
