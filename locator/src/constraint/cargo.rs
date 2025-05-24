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

use crate::ConstraintParseError;

use super::{Comparable, Constraint, Constraints, TryAsSemver};
use either::Either::{Left, Right};
use semver::{
    Comparator as SemVerComparator, Op as SemVerOp, Prerelease as SemVerPrerelease,
    Version as SemVer, VersionReq as SemVerReq,
};

/// A version requirement comparator for Cargo-style SemVer constraints.
///
/// This structure represents a single version comparator in a Cargo constraint,
/// preserving both the specific version components and their precision level.
/// Unlike `semver::Version`, this allows for partial versions (like "1" or "1.2")
/// which are important for correctly implementing Cargo's version comparison semantics.
///
/// ## Intended Usage
///
/// This type is designed to work with the constraint system by:
/// 1. Being convertible to/from `semver::VersionReq` for actual comparisons
/// 2. Supporting the `Comparable` trait to enable constraints matching
/// 3. Preserving version precision information needed for operators like tilde (~)
///
/// It serves as a bridge between Cargo's semver-based constraints and our
/// generic constraint system.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Requirement {
    original: SemVerOp,
    major: u64,
    minor: Option<u64>,
    patch: Option<u64>,
    pre: SemVerPrerelease,
}

impl Requirement {
    /// Converts this comparator into a `VersionReq` with the specified operator.
    ///
    /// Creates a `semver::VersionReq` containing a single comparator with the
    /// given operator and this comparator's version components. This allows
    /// leveraging the semver crate's comparison logic while preserving the
    /// precision information (missing minor/patch).
    fn to_version_req(&self, op: SemVerOp) -> SemVerReq {
        SemVerReq {
            comparators: vec![SemVerComparator {
                op,
                major: self.major,
                minor: self.minor,
                patch: self.patch,
                pre: self.pre.clone(),
            }],
        }
    }

    /// Converts this comparator into a `VersionReq` with its original operator.
    ///
    /// Creates a `semver::VersionReq` containing a single comparator with the
    /// given operator and this comparator's version components. This allows
    /// leveraging the semver crate's comparison logic while preserving the
    /// precision information (missing minor/patch).
    fn as_version_req(&self) -> SemVerReq {
        SemVerReq {
            comparators: vec![SemVerComparator {
                op: self.original,
                major: self.major,
                minor: self.minor,
                patch: self.patch,
                pre: self.pre.clone(),
            }],
        }
    }

    /// Converts this comparator to a complete `semver::Version`.
    ///
    /// Creates a full `Version` using zeros for any missing minor or patch components.
    /// This is primarily used for string representations and comparisons with
    /// non-semver version strings.
    fn to_version(&self) -> SemVer {
        SemVer {
            major: self.major,
            minor: self.minor.unwrap_or(0),
            patch: self.patch.unwrap_or(0),
            pre: self.pre.clone(),
            build: Default::default(),
        }
    }

    /// Returns the string representation of this comparator as a full version.
    ///
    /// This is a convenience method equivalent to `.to_version().to_string()`,
    /// used for efficient comparison with non-semver version strings.
    fn to_version_string(&self) -> String {
        self.to_version().to_string()
    }
}

impl From<SemVerComparator> for Requirement {
    fn from(c: SemVerComparator) -> Self {
        Self {
            original: c.op,
            major: c.major,
            minor: c.minor,
            patch: c.patch,
            pre: c.pre.clone(),
        }
    }
}

/// Implementation of `Comparable` for Comparator against semver::Version.
///
/// This implementation leverages the `semver` crate's operators directly, ensuring
/// correct semantics for all comparison operations, including precision-aware
/// equality and compatibility checks.
impl Comparable<SemVer> for Requirement {
    fn equal(&self, v: &SemVer) -> bool {
        // Use the semver crate's built-in equality operator which
        // handles precision correctly for partial versions
        self.to_version_req(SemVerOp::Exact).matches(v)
    }

    fn less(&self, v: &SemVer) -> bool {
        // Delegate to semver's built-in "less than" operator which
        // properly handles comparisons with version precision
        self.to_version_req(SemVerOp::Less).matches(v)
    }

    fn greater(&self, v: &SemVer) -> bool {
        // Delegate to semver's built-in "greater than" operator
        self.to_version_req(SemVerOp::Greater).matches(v)
    }

    fn compatible(&self, v: &SemVer) -> bool {
        // In the `parse` function, we're mapping multiple operators (tilde, caret, wildcard)
        // to the `Compatible` variant. However, there's more than just this in the actual constraints.
        //
        // For this reason, we also record the _original_ operator and compare using that in this case.
        // We don't want to do this in the general case since it can get confusing if we just outright ignore
        // the `comparable` implementation, but it's sort of unavoidable here unless we want to extend the trait
        // itself to treat all these options as first-class options.
        self.as_version_req().matches(v)
    }
}

impl<S: TryAsSemver> Comparable<S> for Requirement {
    fn compatible(&self, v: &S) -> bool {
        match v.as_semver() {
            Left(version) => self.compatible(&version),
            Right(version) => self.to_version_string().compatible(&version),
        }
    }

    fn equal(&self, v: &S) -> bool {
        match v.as_semver() {
            Left(version) => self.equal(&version),
            Right(version) => self.to_version_string().equal(&version),
        }
    }

    fn less(&self, v: &S) -> bool {
        match v.as_semver() {
            Left(version) => self.less(&version),
            Right(version) => self.to_version_string().less(&version),
        }
    }

    fn greater(&self, v: &S) -> bool {
        match v.as_semver() {
            Left(version) => self.greater(&version),
            Right(version) => self.to_version_string().greater(&version),
        }
    }

    fn less_or_equal(&self, v: &S) -> bool {
        match v.as_semver() {
            Left(version) => self.less_or_equal(&version),
            Right(version) => self.to_version_string().less_or_equal(&version),
        }
    }

    fn greater_or_equal(&self, v: &S) -> bool {
        match v.as_semver() {
            Left(version) => self.greater_or_equal(&version),
            Right(version) => self.to_version_string().greater_or_equal(&version),
        }
    }
}

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
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// // Parse a caret requirement (compatible dependency)
/// let constraints = parse("^1.2.3")?;
/// assert!(constraints.all_match(&Version::new(1.3.0, 0, 0)));
/// assert!(!constraints.all_match(&Version::new(2.0.0, 0, 0)));
///
/// // Parse a complex requirement with multiple constraints
/// let constraints = parse(">= 1.0.0, < 2.0.0, != 1.3.5")?;
/// # Ok(())
/// # }
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
pub fn parse(input: &str) -> Result<Constraints<Requirement>, ConstraintParseError> {
    SemVerReq::parse(input)
        .map_err(ConstraintParseError::InvalidSemver)?
        .comparators
        .into_iter()
        .map(|req| match req.op {
            SemVerOp::Exact => Ok(Constraint::Equal(Requirement::from(req))),
            SemVerOp::Greater => Ok(Constraint::Greater(Requirement::from(req))),
            SemVerOp::GreaterEq => Ok(Constraint::GreaterOrEqual(Requirement::from(req))),
            SemVerOp::Less => Ok(Constraint::Less(Requirement::from(req))),
            SemVerOp::LessEq => Ok(Constraint::LessOrEqual(Requirement::from(req))),
            SemVerOp::Tilde => Ok(Constraint::Compatible(Requirement::from(req))),
            SemVerOp::Caret => Ok(Constraint::Compatible(Requirement::from(req))),
            SemVerOp::Wildcard => Ok(Constraint::Compatible(Requirement::from(req))),
            op => Err(ConstraintParseError::UnhandledSemverOperator(op)),
        })
        .collect::<Result<Vec<_>, ConstraintParseError>>()
        .map(Constraints::from)
}
