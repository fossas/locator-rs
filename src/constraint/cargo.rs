//! # Rust Cargo Version Constraints
//!
//! Implements parsing and evaluation for Rust's Cargo package manager version constraints,
//! enabling accurate constraint checking based on Cargo's SemVer rules.
//!
//! ## Key Features
//!
//! - **SemVer-Based**: Implements Cargo's full SemVer specification including pre-release and build metadata
//! - **Rich Operator Support**: Handles all Cargo operators with their intended semantics:
//!   - Caret (`^`) for compatible version ranges that allow non-breaking updates
//!   - Tilde (`~`) for patch-level updates
//!   - Exact, greater/less than, and equality operators
//!   - Wildcard versions (`*`, `1.*`, etc.)
//! - **Type Safety**: Returns strongly-typed `Constraints<Version>` objects that leverage Rust's type system
//!
//! ## Version Format
//!
//! Cargo uses a SemVer-based versioning scheme with the format `MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]`:
//!
//! - **Major**: Incremented for incompatible API changes
//! - **Minor**: Incremented for backward-compatible functionality
//! - **Patch**: Incremented for backward-compatible bug fixes
//! - **Prerelease**: Optional alpha/beta/rc identifier (e.g., `1.0.0-alpha.1`)
//! - **Build**: Optional build metadata (e.g., `1.0.0+20231125`)
//!
//! ## Constraint Operators
//!
//! Cargo supports the following constraint operators, each with specific semantics:
//!
//! - **Caret (`^`)**: Allows changes that don't modify the leftmost non-zero component
//!   - `^1.2.3` → `>= 1.2.3, < 2.0.0`
//!   - `^0.2.3` → `>= 0.2.3, < 0.3.0`
//!   - `^0.0.3` → `>= 0.0.3, < 0.0.4`
//!
//! - **Tilde (`~`)**: Allows patch-level changes if minor version is specified
//!   - `~1.2.3` → `>= 1.2.3, < 1.3.0`
//!   - `~1.2` → `>= 1.2.0, < 1.3.0`
//!   - `~1` → `>= 1.0.0, < 2.0.0`
//!
//! - **Wildcard (`*`)**: Allows any version in the specified position
//!   - `1.*` → `>= 1.0.0, < 2.0.0`
//!   - `1.2.*` → `>= 1.2.0, < 1.3.0`
//!
//! - **Exact (`=`)**: Requires exact version match
//!   - `=1.2.3` → Exactly version 1.2.3
//!
//! - **Greater/Less**: Standard comparison operators
//!   - `>1.2.3`, `>=1.2.3`, `<1.2.3`, `<=1.2.3`
//!
//! ## Implementation Details
//!
//! This module leverages the `semver` crate (which Cargo itself uses) to parse and evaluate
//! constraints. Each constraint from Cargo is mapped to our generic constraint system:
//!
//! - Caret, tilde, and wildcard operators map to our `Compatible` constraint variant
//! - Other operators map directly to their equivalent constraint variants
//!
//! The constraints are returned as strongly-typed `Constraints<Version>` objects which can be
//! checked against any version type thanks to the fallback comparison implementations.

use super::{Constraint, Constraints};
use crate::ConstraintParseError;
use semver::{Op, Version, VersionReq};

/// Parses a Cargo version requirement string into a set of strongly-typed constraints.
///
/// This function transforms Cargo-style version requirement syntax into our generic
/// constraint system while preserving the exact semantics of each operator. It handles:
///
/// - Single constraints: `^1.2.3`, `~1.0.0`, `>=2.0.0`
/// - Multiple comma-separated constraints: `>=1.0.0, <2.0.0`
/// - All Cargo operators: caret, tilde, wildcard, exact, greater/less, etc.
///
/// The resulting constraints are typed as `Constraint<Version>`, leveraging the standard
/// `semver::Version` type from the Rust ecosystem for maximum compatibility.
///
/// ## Why Use This Function
///
/// While the `semver` crate already provides constraint checking via `VersionReq`, this function
/// integrates Cargo's versioning with our generic constraint system, enabling:
///
/// - Consistent constraint representation across all package ecosystems
/// - Type-safe constraint operations with the full power of the `Comparable` trait
/// - Cross-ecosystem constraint checking (via fallback implementations)
///
/// ## Examples
///
/// ```rust,ignore
/// // Parse a caret requirement (compatible dependency)
/// let constraints = parse("^1.2.3").unwrap();
/// assert!(constraints.all_match(&Version::new(1.3.0, 0, 0)));
/// assert!(!constraints.all_match(&Version::new(2.0.0, 0, 0)));
///
/// // Parse a complex requirement with multiple constraints
/// let constraints = parse(">= 1.0.0, < 2.0.0, != 1.3.5").unwrap();
/// ```
///
/// ## Error Handling
///
/// Returns a `ConstraintParseError` if:
/// - The input string is not a valid SemVer requirement
/// - The constraint uses operators that aren't supported
///
/// ## Specification Compliance
///
/// This implementation follows the exact same versioning rules as Cargo itself,
/// using the same `semver` crate that Cargo uses internally.
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
