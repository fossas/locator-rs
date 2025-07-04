//! # Version Constraints for Package Ecosystems
//!
//! This module provides a unified interface for expressing and evaluating version constraints
//! across different package ecosystems (pip, gem, nuget, etc.) while preserving each ecosystem's
//! specific versioning rules and semantics.
//!
//! ## Design Philosophy
//!
//! The constraint system is designed with these key principles:
//!
//! - **Generic but specific**: Uses Rust's type system to provide a consistent interface while
//!   preserving the ecosystem-specific semantics of each package manager
//! - **Bidirectional comparison**: Enables version constraints to be checked against any type
//!   implementing the necessary traits, handling the complex relationship between constraints and versions
//! - **Composable constraints**: Allows building complex version requirements through
//!   logical combinations of simple constraints
//! - **Extensibility**: Makes it easy to add support for new package ecosystems by
//!   implementing the `Comparable` trait for their version types
//!
//! ## Core Components
//!
//! - [`Constraint<V>`]: A single version constraint (equal, greater than, etc.) where `V` is the constraint's version type
//! - [`Constraints<V>`]: A collection of constraints combined with AND/OR logic
//! - [`Comparable<T>`]: Trait for implementing ecosystem-specific comparison rules
//!
//! ## Package Ecosystem Support
//!
//! Each submodule implements ecosystem-specific version parsing and comparison rules:
//!
//! - `fallback`: Default comparison rules for generic revisions when no specialized implementation exists
//! - `gem`: Ruby Gems version comparison rules (numeric segments, prerelease tags)
//! - `pip`: Python pip version comparison rules (PEP 440 with epochs, dev/pre/post releases)
//! - `nuget`: .NET NuGet version comparison rules (SemVer 1.0/2.0 compatibility)
//! - `cargo`: Rust Cargo version comparison rules (SemVer with extensions)

use derive_new::new;
use documented::Documented;
use either::Either::{self, Left, Right};
use enum_assoc::Assoc;
use nom::{
    AsChar, Finish, IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, multispace0},
    combinator::{eof, map, map_res, opt},
    multi::separated_list0,
    sequence::{delimited, terminated},
};
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

use crate::{Revision, Version};

pub mod cargo;
pub mod fallback;
pub mod gem;
pub mod nuget;
pub mod pip;

/// Enables bidirectional comparison between constraint versions and target versions.
///
/// This trait is the foundation of the generic constraint system, providing the bridge between
/// different versioning schemes while preserving each ecosystem's unique comparison semantics.
///
/// ## How It Works
///
/// The trait's design may appear counterintuitive at first because it reverses the typical
/// comparison direction:
///
/// - `Self` is the constraint's version (e.g., `gem::Version` or `pip::Version`)
/// - Type parameter `V` is the target version being evaluated (e.g., a `Revision`)
/// - Methods like `less()` check if `V` is less than `Self`, not the other way around
///
/// This reversed comparison is intentional and central to how constraints work:
/// - For a constraint like "< 2.0.0", we check if the target version is less than 2.0.0
/// - For a constraint like ">= 1.0.0", we check if the target version is greater than or equal to 1.0.0
///
/// ## Implementation Notes
///
/// When implementing this trait:
///
/// - Implementations map directly to constraint operators (equal, less, greater, compatible)
/// - Each package ecosystem defines its own compatibility rules (tilde, caret, etc.)
/// - Default implementations are provided for derived operations (`not_equal`, `less_or_equal`, etc.)
/// - Cross-type comparison is supported (e.g., comparing `Revision` to `semver::Version`)
///
/// See the ecosystem-specific modules (`pip`, `gem`, `nuget`) for examples of
/// how this trait is implemented for different package ecosystems.
pub trait Comparable<V> {
    /// Implements the "compatible with" constraint (`~=` or `~>` operator).
    ///
    /// This is one of the most complex constraint types because its behavior and syntax
    /// varies significantly across package ecosystems.
    /// Compatibility is generally meant to be used to express "safe upgrade" requirements.
    fn compatible(&self, v: &V) -> bool;

    /// Implements the "exactly equal to" constraint (`==` operator).
    ///
    /// This is the strictest constraint type, requiring exact version equality.
    /// The exact definition of equality depends on the ecosystem:
    ///
    /// - In SemVer: Requires all components to match (major, minor, patch, prerelease)
    /// - In Pip: May normalize certain version forms (e.g., `1.0` and `1.0.0` might be equal)
    /// - In general: Used when a specific version must be used, often for security or compatibility reasons
    fn equal(&self, v: &V) -> bool;

    /// Implements the "less than" constraint (`<` operator).
    ///
    /// Checks if the target version `v` is less than the constraint version `self`.
    /// This is the reversal of typical comparison but makes sense in constraints:
    /// a constraint "< 2.0.0" is satisfied when the target version is less than 2.0.0.
    ///
    /// Used for setting upper bounds on acceptable versions, often to avoid versions
    /// with breaking changes.
    fn less(&self, v: &V) -> bool;

    /// Implements the "greater than" constraint (`>` operator).
    ///
    /// Checks if the target version `v` is greater than the constraint version `self`.
    /// This is the reversal of typical comparison but makes sense in constraints:
    /// a constraint "> 1.0.0" is satisfied when the target version is greater than 1.0.0.
    ///
    /// Used for setting lower bounds on acceptable versions, often to require versions
    /// with certain features or bug fixes.
    fn greater(&self, v: &V) -> bool;

    /// Implements the "not equal to" constraint (`!=` operator).
    ///
    /// Default implementation uses the negation of `equal`.
    /// Primarily used to exclude specific problematic versions.
    fn not_equal(&self, v: &V) -> bool {
        !self.equal(v)
    }

    /// Implements the "less than or equal to" constraint (`<=` operator).
    ///
    /// Default implementation combines `equal` and `less` for efficiency.
    /// Used for setting inclusive upper bounds on acceptable versions.
    fn less_or_equal(&self, v: &V) -> bool {
        self.equal(v) || self.less(v)
    }

    /// Implements the "greater than or equal to" constraint (`>=` operator).
    ///
    /// Default implementation combines `equal` and `greater` for efficiency.
    /// Used for setting inclusive lower bounds on acceptable versions.
    fn greater_or_equal(&self, v: &V) -> bool {
        self.equal(v) || self.greater(v)
    }
}

/// Parse an arbitrary string into a "best effort" set of [`Constraint<Revision>`]:
/// - Supports Semver-like operators: `=`, `==`, `!=`, `>`, `>=`, `<`, `<=`, `~`, `^`
/// - Supports comma-separated constraints in the input string, each forming an AND relationship
/// - Attempts to coerce each version part of the constraint into a reasonable-seeming shape
/// - If all else fails, treats the version components as opaque strings
/// - Any versions that fail to parse are silently dropped, but the returned `Constraints`
///   is guaranteed to have at least one valid constraint if it is `Some`.
///
/// It is generally a much better option to use individual parsers
/// inside of the `constraint` module, for example [`cargo::parse`].
/// However if none of those fit or you aren't sure how to categorize your constraints
/// (e.g. they come from an unknown source) this method does the best it can in the general case.
pub fn parse(input: &str) -> Option<Constraints<Revision>> {
    fn operator(input: &str) -> IResult<&str, &str> {
        alt((
            tag("="),
            tag("=="),
            tag("!="),
            tag(">="),
            tag("<="),
            tag(">"),
            tag("<"),
            tag("^"),
            tag("~"),
            map(
                take_while1(|c: char| !c.is_space() && !c.is_numeric()),
                |_| "^",
            ),
        ))
        .parse(input)
    }

    fn version(input: &str) -> IResult<&str, Revision> {
        map_res(
            take_while1(|c: char| c != ',' && !c.is_space() && !c.is_control()),
            Revision::try_from,
        )
        .parse(input)
    }

    fn single_constraint(input: &str) -> IResult<&str, Constraint<Revision>> {
        map(
            (
                delimited(multispace0, operator, multispace0),
                delimited(multispace0, version, multispace0),
            ),
            |(op, v)| match op {
                "=" | "==" => Constraint::Equal(v),
                "^" | "~" => Constraint::Compatible(v),
                "!=" => Constraint::NotEqual(v),
                ">" => Constraint::Greater(v),
                ">=" => Constraint::GreaterOrEqual(v),
                "<" => Constraint::Less(v),
                "<=" => Constraint::LessOrEqual(v),
                _ => Constraint::Compatible(v),
            },
        )
        .parse(input)
    }

    fn constraints(input: &str) -> IResult<&str, Vec<Constraint<Revision>>> {
        terminated(
            map(
                separated_list0(
                    delimited(multispace0, char(','), multispace0),
                    opt(single_constraint),
                ),
                |constraints| constraints.into_iter().flatten().collect(),
            ),
            eof,
        )
        .parse(input)
    }

    constraints(input.trim())
        .finish()
        .map(|(_, c)| c)
        .ok()
        .and_then(|parsed| {
            if parsed.is_empty() {
                None
            } else {
                Some(Constraints(parsed))
            }
        })
}

/// A strongly-typed version constraint that captures both the operator and target version.
///
/// ## Design Purpose
///
/// `Constraint<V>` provides a unified representation of version constraints across all
/// package ecosystems while preserving type safety and semantic correctness. It acts as
/// a bridge between:
///
/// - Package manager-specific constraint syntax (e.g., `>= 1.0.0`, `~> 2.2`, `!= 3.1.4`)
/// - The internal constraint system that can evaluate whether a version satisfies requirements
///
/// The generic parameter `V` represents the constraint's version type, enabling the
/// constraint system to work with versions from any ecosystem:
///
/// ```
/// # use locator::{Constraint, Revision};
/// # use semver::Version;
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// // A constraint on semver versions
/// let semver_constraint = Constraint::<Version>::Equal(Version::new(1, 0, 0));
///
/// // A constraint on opaque revisions
/// let revision_constraint = Constraint::<Revision>::GreaterOrEqual(Revision::try_from("1.0.0")?);
/// # Ok(())
/// # }
/// ```
///
/// ## Constraint Evaluation
///
/// Each constraint is evaluated by delegating to the corresponding method on the
/// `Comparable` trait implementation for the constrained version type. This allows
/// each ecosystem to define its own comparison semantics:
///
/// ```
/// # use locator::{Constraint, Revision, constraint, revision};
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let pip_constraint = constraint!(Compatible => revision!(1, 2, 3));
/// let version = Revision::try_from("1.2.4")?;
///
/// // This delegates to the `compatible()` method on the `Comparable` implementation
/// // for the version type, which would use Python's compatibility rules for PEP 440
/// assert!(pip_constraint.matches(&version));
/// # Ok(())
/// # }
/// ```
///
/// ## Type Safety and Extensibility
///
/// This design balances type safety with extensibility:
///
/// - The constraint operator (Equal, GreaterThan, etc.) is statically typed via the enum variants
/// - The version comparison semantics are dynamically determined by the `Comparable` implementation
/// - New package ecosystems can be supported by implementing `Comparable` for their version types
///
/// ## Serialization
///
/// The constraint serialization is for transporting this type, not for parsing/serializing
/// the native format used by package managers. The serialized format uses a tagged representation
/// with `kind` and `value` fields to clearly identify the constraint type and its associated version.
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
    Assoc,
    new,
)]
#[schema(example = json!({ "kind": "equal", "value": "1.0.0"}))]
#[serde(rename_all = "snake_case", tag = "kind", content = "value")]
#[func(const fn revision(&self) -> &V)]
#[non_exhaustive]
pub enum Constraint<V> {
    /// Represents a compatibility constraint (`~=` in pip, `~>` in RubyGems, `^` in Cargo).
    ///
    /// This constraint allows versions that are considered "compatible upgrades" to the specified
    /// version, with each ecosystem defining its own compatibility semantics:
    ///
    /// - In PIP: Allows upgrades that don't change the specified parts of the version
    /// - In RubyGems: Allows upgrades that don't change the component before the specified precision
    /// - In SemVer: Allows upgrades that don't change the leftmost non-zero component
    ///
    /// Compatible constraints are commonly used to specify version ranges that should include
    /// bug fixes and minor features but avoid breaking changes.
    #[assoc(revision = &_0)]
    #[new(into)]
    Compatible(V),

    /// Represents an exact equality constraint (`==` operator).
    ///
    /// This is the most restrictive constraint, matching only versions that are exactly
    /// equal to the specified version. Each ecosystem defines its own equality semantics,
    /// particularly regarding how prerelease/build components are handled.
    ///
    /// Used when a specific version is required for compatibility or security reasons.
    #[assoc(revision = &_0)]
    #[new(into)]
    Equal(V),

    /// Represents a not-equal constraint (`!=` operator).
    ///
    /// This constraint matches any version except the specified one. It's commonly
    /// used to exclude known problematic versions from being selected.
    ///
    /// Most useful when combined with other constraints to create complex version requirements.
    #[assoc(revision = &_0)]
    #[new(into)]
    NotEqual(V),

    /// Represents a less-than constraint (`<` operator).
    ///
    /// This constraint matches versions that are strictly less than the specified version.
    /// It's frequently used to set upper bounds on acceptable versions, ensuring that
    /// versions with potential breaking changes are excluded.
    #[assoc(revision = &_0)]
    #[new(into)]
    Less(V),

    /// Represents a less-than-or-equal constraint (`<=` operator).
    ///
    /// This constraint matches versions that are either equal to or less than the specified version.
    /// It's used for setting inclusive upper bounds on acceptable versions.
    #[assoc(revision = &_0)]
    #[new(into)]
    LessOrEqual(V),

    /// Represents a greater-than constraint (`>` operator).
    ///
    /// This constraint matches versions that are strictly greater than the specified version.
    /// It's commonly used to require versions that contain specific features or bug fixes
    /// not present in earlier versions.
    #[assoc(revision = &_0)]
    #[new(into)]
    Greater(V),

    /// Represents a greater-than-or-equal constraint (`>=` operator).
    ///
    /// This constraint matches versions that are either equal to or greater than the specified version.
    /// It's usually used for setting inclusive lower bounds on acceptable versions, ensuring
    /// minimum feature sets or bug fixes are present.
    #[assoc(revision = &_0)]
    #[new(into)]
    GreaterOrEqual(V),
}

impl<V> Constraint<V> {
    /// Evaluates whether a version satisfies this constraint.
    ///
    /// This is the primary method for constraint evaluation and lies at the heart of the
    /// constraint system design. It utilizes the bidirectional comparison capabilities
    /// of the `Comparable` trait to determine if a version meets the constraint's requirements.
    ///
    /// ## How It Works
    ///
    /// 1. The constraint contains both an operator (Equal, Greater, etc.) and a reference version
    /// 2. The reference version must implement `Comparable<T>` for the target version type
    /// 3. The `matches` method dispatches to the appropriate comparison method based on the constraint type
    /// 4. The comparison method determines if the target version satisfies the constraint
    ///
    /// ## Cross-Type Comparisons
    ///
    /// A key feature is the ability to check constraints against different version types.
    /// For example, a constraint based on a SemVer version can be checked against a
    /// string-based revision:
    ///
    /// ```
    /// # use locator::{Constraint, Revision, constraint, revision};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// // Create a constraint requiring exact equality to version 1.0.0
    /// let constraint = constraint!(Equal => revision!(1, 0, 0));
    ///
    /// // Check if string-based versions satisfy this constraint
    /// assert!(constraint.matches(&Revision::try_from("1.0.0")?));
    /// assert!(!constraint.matches(&Revision::try_from("1.0.1")?));
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// This cross-type comparison capability is powered by the ecosystem-specific
    /// implementations of the `Comparable` trait, which handle version coercion and
    /// comparison based on each ecosystem's rules.
    pub fn matches<T>(&self, version: &T) -> bool
    where
        V: Comparable<T>,
    {
        match self {
            Constraint::Compatible(s) => s.compatible(version),
            Constraint::Equal(s) => s.equal(version),
            Constraint::NotEqual(s) => s.not_equal(version),
            Constraint::Less(s) => s.less(version),
            Constraint::LessOrEqual(s) => s.less_or_equal(version),
            Constraint::Greater(s) => s.greater(version),
            Constraint::GreaterOrEqual(s) => s.greater_or_equal(version),
        }
    }

    /// Transforms the constraint's inner version by reference, preserving the constraint type.
    ///
    /// This method enables constraint type conversions without cloning the inner version,
    /// which is particularly useful for adapting constraints between different version
    /// representations. It maintains the original constraint operator (Equal, Greater, etc.)
    /// while transforming the inner version reference through the provided closure.
    ///
    /// ## Use Cases
    ///
    /// - Converting between different version representations (string, semver, etc.)
    /// - Creating debugging representations of constraints
    /// - Transforming constraints for serialization
    ///
    /// ## Example
    ///
    /// ```
    /// # use locator::{Constraint, Revision, constraint, revision};
    /// # use std::fmt::Debug;
    /// // Convert a Revision constraint to a string-based constraint
    /// let rev_constraint = constraint!(Equal => revision!(1, 0, 0));
    /// let string_constraint = rev_constraint.map_ref(|r: &Revision| format!("{:?}", r));
    /// ```
    ///
    /// This transformation preserves the constraint type (Equal in this case) while
    /// converting the inner version to a string representation.
    pub fn map_ref<'a, R, F: Fn(&'a V) -> R>(&'a self, closure: F) -> Constraint<R> {
        match self {
            Constraint::Compatible(v) => Constraint::Compatible(closure(v)),
            Constraint::Equal(v) => Constraint::Equal(closure(v)),
            Constraint::NotEqual(v) => Constraint::NotEqual(closure(v)),
            Constraint::Less(v) => Constraint::Less(closure(v)),
            Constraint::LessOrEqual(v) => Constraint::LessOrEqual(closure(v)),
            Constraint::Greater(v) => Constraint::Greater(closure(v)),
            Constraint::GreaterOrEqual(v) => Constraint::GreaterOrEqual(closure(v)),
        }
    }

    /// Transforms the constraint's inner version by consuming it, preserving the constraint type.
    ///
    /// This method is similar to `map_ref` but takes ownership of the constraint and transforms
    /// the inner version by value. It's ideal for conversions that need to fully own and transform
    /// the version while maintaining the constraint operator.
    ///
    /// ## Use Cases
    ///
    /// - Type conversions that require ownership of the source value
    /// - Converting between incompatible version types that can't be referenced
    /// - Building derived constraint systems with transformed versions
    ///
    /// This allows for seamless conversion between constraint types, enabling
    /// interoperability between different version constraint systems while
    /// maintaining the constraint semantics.
    pub fn map<R>(self, closure: impl Fn(V) -> R) -> Constraint<R> {
        match self {
            Constraint::Compatible(v) => Constraint::Compatible(closure(v)),
            Constraint::Equal(v) => Constraint::Equal(closure(v)),
            Constraint::NotEqual(v) => Constraint::NotEqual(closure(v)),
            Constraint::Less(v) => Constraint::Less(closure(v)),
            Constraint::LessOrEqual(v) => Constraint::LessOrEqual(closure(v)),
            Constraint::Greater(v) => Constraint::Greater(closure(v)),
            Constraint::GreaterOrEqual(v) => Constraint::GreaterOrEqual(closure(v)),
        }
    }
}

impl<V: std::fmt::Debug> std::fmt::Display for Constraint<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constraint::Compatible(v) => write!(f, "~={v:?}"),
            Constraint::Equal(v) => write!(f, "=={v:?}"),
            Constraint::NotEqual(v) => write!(f, "!={v:?}"),
            Constraint::Less(v) => write!(f, "<{v:?}"),
            Constraint::LessOrEqual(v) => write!(f, "<={v:?}"),
            Constraint::Greater(v) => write!(f, ">{v:?}"),
            Constraint::GreaterOrEqual(v) => write!(f, ">={v:?}"),
        }
    }
}

impl<V: Clone> From<&Constraint<V>> for Constraint<V> {
    fn from(c: &Constraint<V>) -> Self {
        c.clone()
    }
}

impl<V> AsRef<Constraint<V>> for Constraint<V> {
    fn as_ref(&self) -> &Constraint<V> {
        self
    }
}

impl<V> AsRef<V> for Constraint<V> {
    fn as_ref(&self) -> &V {
        match self {
            Constraint::Compatible(v) => v,
            Constraint::Equal(v) => v,
            Constraint::NotEqual(v) => v,
            Constraint::Less(v) => v,
            Constraint::LessOrEqual(v) => v,
            Constraint::Greater(v) => v,
            Constraint::GreaterOrEqual(v) => v,
        }
    }
}

/// A composable collection of version constraints with flexible evaluation logic.
///
/// ## Design Purpose
///
/// `Constraints<V>` enables the expression of complex version requirements by combining
/// multiple individual constraints. It mirrors how real-world package requirements are
/// typically specified across package ecosystems:
///
/// - In pip: `">=1.0.0,<2.0.0,!=1.3.5"`
/// - In gem: `">= 1.0.0", "< 2.0.0", "!= 1.3.5"`
/// - In Cargo: `">= 1.0.0, < 2.0.0, != 1.3.5"`
///
/// These collections need to be evaluated with different logical combinators:
/// - AND semantics (all constraints must match) for typical version ranges
/// - OR semantics (any constraint may match) for alternative versions
///
/// ## Flexibility Through Composition
///
/// The design allows building complex version requirements like:
///
/// ```
/// # use locator::{Constraint, Constraints, Error, Revision, constraint, revision, constraints};
/// # fn main() -> Result<(), Error> {
/// // A range constraint: >= 1.0.0 AND < 2.0.0
/// let range = constraints!(
///     { GreaterOrEqual => revision!(1, 0, 0) },
///     { Less => revision!(2, 0, 0) }
/// );
///
/// // Version 1.5.0 satisfies both constraints
/// assert!(range.all_match(&Revision::try_from("1.5.0")?));
///
/// // Version 2.5.0 fails the < 2.0.0 constraint
/// assert!(!range.all_match(&Revision::try_from("2.5.0")?));
/// # Ok(())
/// # }
/// ```
///
/// ## Common Use Patterns
///
/// While evaluating all constraints (AND logic) is the most common use case in package
/// managers, the system also supports OR evaluation for scenarios like:
///
/// - Offering alternative compatible versions
/// - Supporting multiple version formats
/// - Creating complex conditions (through combination of AND/OR operations)
///
/// This flexibility makes it possible to model the full richness of version
/// requirement expressions across different package ecosystems.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, Documented, ToSchema)]
#[non_exhaustive]
pub struct Constraints<V>(Vec<Constraint<V>>);

impl<V> Constraints<V> {
    /// Iterate over constraints in the set.
    pub fn iter(&self) -> impl Iterator<Item = &Constraint<V>> {
        self.0.iter()
    }

    /// Unpack into an iterator of constraints.
    #[allow(
        clippy::should_implement_trait,
        reason = "We support `From<impl IntoIterator<...>>` so this conflicts if we implement the trait"
    )]
    pub fn into_iter(self) -> impl Iterator<Item = Constraint<V>> {
        self.0.into_iter()
    }

    /// Evaluates if a version satisfies all constraints in the collection (AND logic).
    ///
    /// This method implements the most common constraint evaluation model in package managers:
    /// requiring a version to satisfy multiple constraints simultaneously. It's ideal for
    /// representing version ranges with both lower and upper bounds, or for excluding specific
    /// problematic versions within a range.
    ///
    /// ## Package Manager Equivalents
    ///
    /// - **Pip**: `">=1.0.0,<2.0.0,!=1.3.5"` (comma-separated constraints)
    /// - **RubyGems**: `">= 1.0.0", "< 2.0.0", "!= 1.3.5"` (multiple constraints)
    /// - **Cargo**: `">= 1.0.0, < 2.0.0, != 1.3.5"` (comma-separated constraints)
    ///
    /// ## Implementation Details
    ///
    /// The method uses short-circuit evaluation for efficiency - if any constraint fails to match,
    /// the method immediately returns false without checking remaining constraints.
    ///
    /// ## Example
    ///
    /// ```
    /// # use locator::{Constraints, Error, Revision, constraints, revision};
    /// # fn main() -> Result<(), Error> {
    /// // Define a constraint range: versions greater than 1.0.0 AND less than 2.0.0
    /// let range = constraints!(
    ///     { Greater => revision!(1, 0, 0) },
    ///     { Less => revision!(2, 0, 0) }
    /// );
    ///
    /// // Version 1.5.0 satisfies both constraints (it's > 1.0.0 AND < 2.0.0)
    /// assert!(range.all_match(&Revision::try_from("1.5.0")?));
    ///
    /// // Version 0.9.0 doesn't satisfy the first constraint (it's not > 1.0.0)
    /// assert!(!range.all_match(&Revision::try_from("0.9.0")?));
    /// # Ok(())
    /// # }
    /// ```
    pub fn all_match<T>(&self, version: &T) -> bool
    where
        V: Comparable<T>,
    {
        for constraint in self.iter() {
            if !constraint.matches(version) {
                return false;
            }
        }
        true
    }

    /// Evaluates if a version satisfies any constraint in the collection (OR logic).
    ///
    /// This method provides an alternative constraint evaluation model that's useful for
    /// scenarios where multiple different version options are acceptable. While less common
    /// in package manager syntax, OR logic is conceptually important for expressing more
    /// complex version requirements.
    ///
    /// ## Use Cases
    ///
    /// - **Alternative versions**: When specific discrete versions are allowed
    /// - **Complex requirements**: When combined with AND logic to create sophisticated patterns
    /// - **Version migration**: When supporting both old and new version schemes
    ///
    /// ## Implementation Details
    ///
    /// The method uses short-circuit evaluation for efficiency - as soon as any constraint matches,
    /// the method immediately returns true without checking remaining constraints.
    ///
    /// ## Example
    ///
    /// ```
    /// # use locator::{Constraints, Error, Revision, constraints, revision};
    /// # fn main() -> Result<(), Error> {
    /// // Define a set of acceptable discrete versions: exactly 1.0.0 OR exactly 2.0.0
    /// let options = constraints!(
    ///     { Equal => revision!(1, 0, 0) },
    ///     { Equal => revision!(2, 0, 0) }
    /// );
    ///
    /// // Either specific version is acceptable
    /// assert!(options.any_match(&Revision::try_from("1.0.0")?));
    /// assert!(options.any_match(&Revision::try_from("2.0.0")?));
    ///
    /// // But other versions are not
    /// assert!(!options.any_match(&Revision::try_from("1.5.0")?));
    /// assert!(!options.any_match(&Revision::try_from("3.0.0")?));
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Advanced Patterns
    ///
    /// While not directly supported by the API, combining collections with AND/OR logic
    /// can create powerful constraint patterns. For instance, you could model:
    /// "Must be either (>= 1.0.0 AND < 2.0.0) OR (>= 3.0.0 AND < 4.0.0)"
    /// by creating two separate `Constraints` collections and checking if a version
    /// satisfies all constraints in either collection.
    pub fn any_match<T>(&self, version: &T) -> bool
    where
        V: Comparable<T>,
    {
        for constraint in self.iter() {
            if constraint.matches(version) {
                return true;
            }
        }
        false
    }
}

impl<I, T, V> From<I> for Constraints<V>
where
    I: IntoIterator<Item = T>,
    T: Into<Constraint<V>>,
{
    fn from(constraints: I) -> Self {
        Self(constraints.into_iter().map(Into::into).collect())
    }
}

impl<V> From<Constraint<V>> for Constraints<V> {
    fn from(constraint: Constraint<V>) -> Self {
        Self(vec![constraint])
    }
}

impl<V: Clone> From<&Constraint<V>> for Constraints<V> {
    fn from(constraint: &Constraint<V>) -> Self {
        constraint.clone().into()
    }
}

impl<V: Clone> AsRef<Constraints<V>> for Constraints<V> {
    fn as_ref(&self) -> &Constraints<V> {
        self
    }
}

/// Construct a [`Constraint<Revision>`](Constraint), guaranteed to be valid at compile time.
///
/// ```
/// # use locator::{Constraint, Revision, Version};
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let constraint = locator::constraint!(Compatible => locator::revision!(1, 0, 0));
/// let expected = Constraint::Compatible(Revision::try_from("1.0.0")?);
/// assert_eq!(constraint, expected);
///
/// let constraint = locator::constraint!(Equal => locator::revision!("abcd1234"));
/// let expected = Constraint::Equal(Revision::try_from("abcd1234")?);
/// assert_eq!(constraint, expected);
/// # Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! constraint {
    ($variant:ident => $($tail:tt)*) => {
        $crate::Constraint::$variant($($tail)*)
    };
}

/// Construct multiple [`Constraints<Revision>`](Constraints), guaranteed to be valid at compile time.
///
/// ```
/// # use locator::{Constraint, Constraints, Revision};
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let constraint = locator::constraints!(
///     { Compatible => locator::revision!(1, 0, 0) },
///     { Compatible => locator::revision!(1, 1, 0) },
/// );
/// let expected = Constraints::from(vec![
///     Constraint::Compatible(locator::revision!(1, 0, 0)),
///     Constraint::Compatible(locator::revision!(1, 1, 0)),
/// ]);
/// assert_eq!(constraint, expected);
///
/// let constraint = locator::constraints!(
///     { Equal => locator::revision!("abcd1234") },
///     { Equal => locator::revision!("abcd12345") },
/// );
/// let expected = Constraints::from(vec![
///     Constraint::Equal(Revision::try_from("abcd1234")?),
///     Constraint::Equal(Revision::try_from("abcd12345")?),
/// ]);
/// assert_eq!(constraint, expected);
/// # Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! constraints {
    ($({ $variant:ident => $($revision:tt)* }),* $(,)?) => {
        $crate::Constraints::from(vec![
            $(
                $crate::Constraint::$variant($($revision)*)
            ),*
        ])
    };
}

/// Supports attempting to coerce a type into a [`semver::Version`].
pub trait TryAsSemver {
    /// Attempt to coerce the instance into [`semver::Version`].
    ///
    /// The `Either::Right` variant is the original input in the case
    /// that the input is not a valid semver version.
    fn as_semver(&self) -> Either<semver::Version, &str>;
}

impl TryAsSemver for Version {
    fn as_semver(&self) -> Either<semver::Version, &str> {
        match &self.parsed {
            versions::Versioning::Ideal(semver) => Left(semver::Version {
                major: semver.major as u64,
                minor: semver.minor as u64,
                patch: semver.patch as u64,
                build: semver
                    .meta
                    .as_ref()
                    .and_then(|m| semver::BuildMetadata::new(m.as_str()).ok())
                    .unwrap_or(semver::BuildMetadata::EMPTY),
                pre: semver
                    .pre_rel
                    .as_ref()
                    .and_then(|s| semver::Prerelease::new(s.to_string().as_str()).ok())
                    .unwrap_or(semver::Prerelease::EMPTY),
            }),
            _ => match semver::Version::parse(self.input.as_str()).ok() {
                Some(version) => Left(version),
                None => Right(self.input.as_str()),
            },
        }
    }
}

impl TryAsSemver for Revision {
    fn as_semver(&self) -> Either<semver::Version, &str> {
        match self {
            Revision::Version(version) => version.as_semver(),
            Revision::Opaque(version) => Right(version.as_str()),
        }
    }
}
