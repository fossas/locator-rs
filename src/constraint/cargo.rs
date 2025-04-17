//! Handling of Cargo-specific constraints.
//!
//! **IMPORTANT**: The constraint "parsing" and "checking" done here are superficial and are not
//! independently valuable outside of the [`crate::constraint`] abstractions. This is because the
//! [`semver`] crate already offers canonical constraint parsing and checking functionality. The
//! implementations here simply serve to maintain the larger [`Constraint`] and [`Constraints`]
//! interfaces while internally disregarding them in favour of [`semver::VersionReq`].

use super::{Constraint, Constraints};
use crate::{ConstraintParseError, Revision};
use semver::{Op, Version, VersionReq};
use tap::pipe::Pipe;
use thiserror::Error;

/// Check if a revision satisfies a constraint.
///
/// Conveniently, the `semver` crate implements Cargo's flavor of SemVer (see:
/// https://docs.rs/semver/latest/semver/index.html) This means we can rely on
/// [`semver::VersionReq::matches`] to determine if the revision satisfies the
/// constraint.
///
/// **WARNING**: This function expects that the given [`Constraint`] is one of a
/// set of [`Constraints`] constructed by [`crate::constraint::cargo::parse`].
/// Please read the documentation for that function for more information.
pub fn compare(constraint: &Constraint, revision: &Revision) -> Result<bool, CargoCompareError> {
    let version = Version::parse(&revision.as_str()).map_err(CargoCompareError::InvalidSemver)?;
    let revision = constraint.revision();

    let Revision::Opaque(req) = revision else {
        return Err(CargoCompareError::UnexpectedSemverRevision(
            revision.clone(),
        ));
    };

    let req = VersionReq::parse(req).map_err(CargoCompareError::InvalidSemver)?;

    Ok(req.matches(&version))
}

/// Parse a string into a set of constraints.
///
/// <b>WARNING</b>: This function is provided only for consistency within the
/// [`crate::constraint`] module. The [`Constraints`] returned are <b>not semantically
/// correct</b> and should not be interpreted outside of their use within this
/// module. This is because a Cargo revision can already be checked against a constraint
/// via [`semver::VersionReq::matches`], so this function simply stores each
/// [`semver::Comparator`] string from the parsed [`VersionReq`] in an arbitrary
/// [`Constraint`] variant.
pub fn parse(str: &str) -> Result<Constraints, ConstraintParseError> {
    let req = VersionReq::parse(str).map_err(ConstraintParseError::InvalidSemver)?;

    req.comparators
        .into_iter()
        .map(|comparator| {
            let revision = Revision::Opaque(comparator.to_string());
            match comparator.op {
                Op::Exact => Constraint::Equal(revision),
                Op::Greater => Constraint::Greater(revision),
                Op::GreaterEq => Constraint::GreaterOrEqual(revision),
                Op::Less => Constraint::Less(revision),
                Op::LessEq => Constraint::LessOrEqual(revision),
                Op::Tilde => Constraint::Compatible(revision),
                Op::Caret => Constraint::Compatible(revision),
                Op::Wildcard => Constraint::Compatible(revision),
                _ => unreachable!(),
            }
        })
        .collect::<Vec<_>>()
        .pipe(Constraints::from)
        .pipe(Ok)
}

#[derive(Error, Debug)]
pub enum CargoCompareError {
    #[error("`cargo::compare` should only be called with opaque revisions, got: {0}")]
    UnexpectedSemverRevision(Revision),

    #[error(transparent)]
    InvalidSemver(#[from] semver::Error),
}
