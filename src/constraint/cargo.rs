//! # Rust Cargo Version Constraints
//!
//! Implements parsing and comparison for Rust's Cargo package manager's 
//! version constraints. This module enables handling of Cargo's semver-based 
//! version requirements.
//!
//! ## Key Features
//!
//! - **SemVer Support**: Fully implements Cargo's flavor of SemVer
//! - **Compatible Ranges**: Handles caret (`^`), tilde (`~`), and other Cargo range operators
//! - **Rust Ecosystem Integration**: Properly handles Rust's packaging conventions
//!
//! ## Core Components
//!
//! The module provides two main functions:
//! - `parse_semver`: Parses a Cargo version constraint into `Constraints<Version>`
//! - `parse`: Legacy parser returning generic constraints for compatibility
//! - `compare`: Validates if a revision matches a constraint
//!
//! ## Implementation Notes
//!
//! This module leverages the `semver` crate which already implements Cargo's version
//! specification. We wrap its functionality to integrate with our generic constraint system.

use super::{Comparable, Constraint, Constraints};
use crate::{ConstraintParseError, Revision};
use semver::{Op, Version, VersionReq};
use thiserror::Error;
use tracing::warn;

/// Parse a string into a set of constraints using semver's Version type directly.
///
/// This function parses a Cargo version requirement string (e.g., "^1.2.3", "~1.0", ">=2.0.0, <3.0.0")
/// and converts it into a set of strongly-typed `Constraint<Version>` objects.
///
/// ## Example
///
/// ```rust,ignore
/// let constraints = parse_semver("^1.2.3").unwrap();
/// ```
///
/// This will create a constraint that matches any version compatible with 1.2.3
/// according to Cargo's semver rules.
pub fn parse_semver(input: &str) -> Result<Constraints<Version>, ConstraintParseError> {
    let req = VersionReq::parse(input).map_err(ConstraintParseError::InvalidSemver)?;

    req.comparators
        .into_iter()
        .map(|comparator| {
            let version = Version {
                major: comparator.major,
                minor: comparator.minor.unwrap_or(0),
                patch: comparator.patch.unwrap_or(0),
                pre: comparator.pre,
                build: Default::default(),
            };
            
            match comparator.op {
                Op::Exact => Ok(Constraint::Equal(version)),
                Op::Greater => Ok(Constraint::Greater(version)),
                Op::GreaterEq => Ok(Constraint::GreaterOrEqual(version)),
                Op::Less => Ok(Constraint::Less(version)),
                Op::LessEq => Ok(Constraint::LessOrEqual(version)),
                Op::Tilde => Ok(Constraint::Compatible(version)),
                Op::Caret => Ok(Constraint::Compatible(version)),
                Op::Wildcard => Ok(Constraint::Compatible(version)),
                _ => Err(ConstraintParseError::UnhandledSemverOperator(comparator.op)),
            }
        })
        .collect::<Result<Vec<_>, ConstraintParseError>>()
        .map(Constraints::from)
}

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
#[tracing::instrument]
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
/// **WARNING**: This function is provided only for compatibility with the old 
/// constraint system. Prefer using [`parse_semver`] for new code as it returns
/// strongly-typed constraints that are more type-safe and efficient.
///
/// This function stores each [`semver::Comparator`] string from the parsed 
/// [`VersionReq`] in a [`Constraint`] with an opaque revision.
#[tracing::instrument]
pub fn parse(str: &str) -> Result<Constraints, ConstraintParseError> {
    let req = VersionReq::parse(str).map_err(ConstraintParseError::InvalidSemver)?;

    req.comparators
        .into_iter()
        .map(|comparator| {
            let revision = Revision::Opaque(comparator.to_string());
            match comparator.op {
                Op::Exact => Ok(Constraint::Equal(revision)),
                Op::Greater => Ok(Constraint::Greater(revision)),
                Op::GreaterEq => Ok(Constraint::GreaterOrEqual(revision)),
                Op::Less => Ok(Constraint::Less(revision)),
                Op::LessOrEqual => Ok(Constraint::LessOrEqual(revision)),
                Op::Tilde => Ok(Constraint::Compatible(revision)),
                Op::Caret => Ok(Constraint::Compatible(revision)),
                Op::Wildcard => Ok(Constraint::Compatible(revision)),
                _ => Err(ConstraintParseError::UnhandledSemverOperator(comparator.op)),
            }
        })
        .collect::<Result<Vec<_>, ConstraintParseError>>()
        .map(Constraints::from)
}

#[derive(Error, Debug)]
pub enum CargoCompareError {
    #[error("`cargo::compare` should only be called with opaque revisions, got: {0}")]
    UnexpectedSemverRevision(Revision),

    #[error(transparent)]
    InvalidSemver(#[from] semver::Error),
}
