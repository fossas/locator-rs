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
//! ## Implementation Notes
//!
//! This module leverages the `semver` crate which already implements Cargo's version
//! specification. We wrap its functionality to integrate with our generic constraint system.
//! The fallback comparison implementations automatically handle comparing `Constraint<Version>`
//! against `Revision` types.

use super::{Constraint, Constraints};
use crate::ConstraintParseError;
use semver::{Op, Version, VersionReq};

/// Parse a string into a set of constraints using semver's Version type directly.
///
/// This function parses a Cargo version requirement string (e.g., "^1.2.3", "~1.0", ">=2.0.0, <3.0.0")
/// and converts it into a set of strongly-typed `Constraint<Version>` objects.
///
/// ## Example
///
/// ```rust,ignore
/// let constraints = parse("^1.2.3").unwrap();
/// ```
///
/// This will create a constraint that matches any version compatible with 1.2.3
/// according to Cargo's semver rules.
pub fn parse(input: &str) -> Result<Constraints<Version>, ConstraintParseError> {
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
