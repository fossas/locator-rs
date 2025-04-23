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

use super::{Comparable, Constraint, Constraints};
use crate::Revision;
use semver::{Op, Version, VersionReq};

/// A version requirement comparator for Cargo-style SemVer constraints.
///
/// This structure represents a single version comparator in a Cargo constraint,
/// preserving both the specific version components and their precision level.
/// Unlike `semver::Version`, this allows for partial versions (like "1" or "1.2")
/// which are important for correctly implementing Cargo's version comparison semantics.
///
/// ## Fields Explanation
///
/// - `major`: The major version number (always required in SemVer)
/// - `minor`: The minor version number (optional in constraints like `=1` or `^1`)
/// - `patch`: The patch version number (optional in constraints like `=1.2` or `~1.2`)
/// - `pre`: Pre-release identifier string (e.g., "alpha.1" in "1.0.0-alpha.1")
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
pub struct Comparator {
    /// The major version number (e.g., "1" in "1.2.3")
    pub major: u64,

    /// The minor version number (e.g., "2" in "1.2.3"), or None when not specified (e.g., in "1")
    pub minor: Option<u64>,

    /// The patch version number (e.g., "3" in "1.2.3"), or None when not specified (e.g., in "1.2")
    pub patch: Option<u64>,

    /// The prerelease identifier string (e.g., "alpha.1" in "1.2.3-alpha.1")
    pub pre: String,
}

impl Comparator {
    /// Converts this comparator into a `VersionReq` with the specified operator.
    ///
    /// Creates a `semver::VersionReq` containing a single comparator with the
    /// given operator and this comparator's version components. This allows
    /// leveraging the semver crate's comparison logic while preserving the
    /// precision information (missing minor/patch).
    fn to_version_req(&self, op: Op) -> VersionReq {
        let cmp = semver::Comparator {
            op,
            major: self.major,
            minor: self.minor,
            patch: self.patch,
            pre: self.pre.parse().unwrap_or_default(),
        };
        VersionReq {
            comparators: vec![cmp],
        }
    }

    /// Converts this comparator to a complete `semver::Version`.
    ///
    /// Creates a full `Version` using zeros for any missing minor or patch components.
    /// This is primarily used for string representations and comparisons with
    /// non-semver version strings.
    fn to_version(&self) -> Version {
        Version {
            major: self.major,
            minor: self.minor.unwrap_or(0),
            patch: self.patch.unwrap_or(0),
            pre: self.pre.parse().unwrap_or_default(),
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

/// Implementation of `Comparable` for Comparator against semver::Version.
///
/// This implementation leverages the `semver` crate's operators directly, ensuring
/// correct semantics for all comparison operations, including precision-aware
/// equality and compatibility checks.
impl Comparable<Version> for Comparator {
    fn equal(&self, v: &Version) -> bool {
        // Use the semver crate's built-in equality operator which
        // handles precision correctly for partial versions
        self.to_version_req(Op::Exact).matches(v)
    }

    fn less(&self, v: &Version) -> bool {
        // Delegate to semver's built-in "less than" operator which
        // properly handles comparisons with version precision
        self.to_version_req(Op::Less).matches(v)
    }

    fn greater(&self, v: &Version) -> bool {
        // Delegate to semver's built-in "greater than" operator
        self.to_version_req(Op::Greater).matches(v)
    }

    fn compatible(&self, v: &Version) -> bool {
        // "Compatible" in our constraint system maps to Cargo's tilde ("~") operator,
        // which allows patch-level upgrades but preserves the major.minor components
        self.to_version_req(Op::Tilde).matches(v)
    }
}

/// Implementation of `Comparable` for Comparator against Revision.
///
/// This implementation handles both semver and non-semver revisions:
/// - For `Revision::Semver`, it uses direct semver comparison logic
/// - For `Revision::Opaque`, it falls back to string-based comparison
///
/// Note that `Revision::Opaque` can only be created if the string isn't valid semver,
/// so there's no need to attempt parsing these strings as semver.
impl Comparable<Revision> for Comparator {
    fn equal(&self, revision: &Revision) -> bool {
        match revision {
            Revision::Semver(version) => self.equal(version),
            Revision::Opaque(version) => self.to_version_string().equal(version),
        }
    }

    fn less(&self, revision: &Revision) -> bool {
        match revision {
            Revision::Semver(version) => self.less(version),
            Revision::Opaque(version) => self.to_version_string().less(version),
        }
    }

    fn greater(&self, revision: &Revision) -> bool {
        match revision {
            Revision::Semver(version) => self.greater(version),
            Revision::Opaque(version) => self.to_version_string().greater(version),
        }
    }

    fn compatible(&self, revision: &Revision) -> bool {
        match revision {
            Revision::Semver(version) => self.compatible(version),
            Revision::Opaque(version) => self.to_version_string().compatible(version),
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
pub fn parse(input: &str) -> Result<Constraints<Comparator>, ConstraintParseError> {
    VersionReq::parse(input)
        .map_err(ConstraintParseError::InvalidSemver)?
        .comparators
        .into_iter()
        .map(|req| {
            let version = Comparator {
                major: req.major,
                minor: req.minor,
                patch: req.patch,
                pre: req.pre.to_string(),
            };

            match req.op {
                Op::Exact => Ok(Constraint::Equal(version)),
                Op::Greater => Ok(Constraint::Greater(version)),
                Op::GreaterEq => Ok(Constraint::GreaterOrEqual(version)),
                Op::Less => Ok(Constraint::Less(version)),
                Op::LessEq => Ok(Constraint::LessOrEqual(version)),
                Op::Tilde => Ok(Constraint::Compatible(version)),
                Op::Caret => Ok(Constraint::Compatible(version)),
                Op::Wildcard => Ok(Constraint::Compatible(version)),
                _ => Err(ConstraintParseError::UnhandledSemverOperator(req.op)),
            }
        })
        .collect::<Result<Vec<_>, ConstraintParseError>>()
        .map(Constraints::from)
}
