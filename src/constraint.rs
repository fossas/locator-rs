use std::cmp::Ordering;

use documented::Documented;
use enum_assoc::Assoc;
use lexical_sort::lexical_cmp;
use serde::{Deserialize, Serialize};
use strum::Display;
use unicase::UniCase;
use utoipa::ToSchema;

use crate::{CompareError, Error, Fetcher, Revision};

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
)]
#[schema(example = json!("v1.0.0"))]
#[serde(rename_all = "snake_case", tag = "kind", content = "value")]
#[func(const fn revision(&self) -> &Revision)]
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
    /// Compare the constraint to the given revision according to the rules of the provided fetcher.
    /// The default if there are no additional rules specified for the fetcher is:
    /// - If both versions are semver, compare according to semver rules.
    /// - If not, coerce them both to an opaque string and compare according to unicode ordering rules.
    ///
    /// Inicode in this instance [`Constraint::Compatible`] is a case-insensitive equality comparison.
    pub fn compare(&self, fetcher: Fetcher, rev: &Revision) -> bool {
        // In the future we'll have a bunch of fetcher-specific comparisons here.
        // match fetcher {
        //     ...
        // }

        // Fallback: if they're both semver, compare according to semver rules.
        if let (Revision::Semver(c), Revision::Semver(r)) = (self.revision(), rev) {
            todo!()
        }

        // Final fallback: if compare according to UTF8 rules.
        match self {
            Constraint::Compatible(c) => UniCase::new(c.as_str()) == UniCase::new(rev.as_str()),
            _ => self.lexically_compare(rev),
        }
    }

    fn as_ords(&self) -> Vec<Ordering> {
        match self {
            Constraint::Compatible(_) => vec![Ordering::Equal],
            Constraint::Equal(_) => vec![Ordering::Equal],
            Constraint::NotEqual(_) => vec![Ordering::Less, Ordering::Greater],
            Constraint::Less(_) => vec![Ordering::Less],
            Constraint::LessOrEqual(_) => vec![Ordering::Less, Ordering::Equal],
            Constraint::Greater(_) => vec![Ordering::Greater],
            Constraint::GreaterOrEqual(_) => vec![Ordering::Greater, Ordering::Equal],
        }
    }

    fn lexically_compare(&self, other: impl ToString) -> bool {
        let constraint = self.revision().to_string();
        let other = other.to_string();
        for ord in self.as_ords() {
            if lexical_sort::lexical_cmp(&constraint, &other) == ord {
                return true;
            }
        }
        return false;
    }
}

impl From<&Constraint> for Constraint {
    fn from(c: &Constraint) -> Self {
        c.clone()
    }
}

impl From<Constraint> for Revision {
    fn from(c: Constraint) -> Self {
        match c {
            Constraint::Equal(r) => r,
            Constraint::NotEqual(r) => r,
            Constraint::Less(r) => r,
            Constraint::LessOrEqual(r) => r,
            Constraint::Greater(r) => r,
            Constraint::GreaterOrEqual(r) => r,
            Constraint::Compatible(r) => r,
        }
    }
}

impl From<&Constraint> for Revision {
    fn from(c: &Constraint) -> Self {
        c.clone().into()
    }
}

/// A set of [`Constraint`].
///
/// This set forms an "AND" relationship between its constraints:
/// it implements [`Constrainable::compare`] such that it indicates success
/// only if all constraints are satisified.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, Documented, ToSchema)]
#[non_exhaustive]
pub struct Constraints(Vec<Constraint>);

impl Constraints {
    /// Iterate over constraints in the set.
    pub fn iter(&self) -> impl Iterator<Item = &Constraint> {
        self.0.iter()
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
