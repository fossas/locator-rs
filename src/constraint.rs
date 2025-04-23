//! Version constraints and comparisons for package version requirements.
//!
//! This module provides a generic system for expressing and comparing version constraints
//! across different package ecosystems (pip, gem, nuget, etc.). Each ecosystem can define
//! its own versioning rules while maintaining a consistent constraint interface.
//!
//! ## Core Components
//!
//! - [`Constraint<V>`]: A single version constraint (equal, greater than, etc.)
//! - [`Constraints<V>`]: A collection of constraints combined with AND/OR logic
//! - [`Comparable<T>`]: Trait for implementing ecosystem-specific comparison rules
//!
//! ## Package Ecosystem Support
//!
//! Each submodule implements ecosystem-specific version parsing and comparison:
//!
//! - `fallback`: Default comparison rules for generic revisions
//! - `gem`: Ruby gems versioning rules
//! - `pip`: Python pip versioning rules
//! - `nuget`: .NET NuGet versioning rules

use derive_new::new;
use documented::Documented;
use enum_assoc::Assoc;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

pub mod fallback;
pub mod gem;
pub mod nuget;
pub mod pip;

/// Enables version constraint validation against different version types.
///
/// This trait is the foundation of the generic constraint system, allowing different
/// package ecosystems to define their own versioning rules while maintaining a consistent API.
/// When implementing this trait:
///
/// - `Self` represents a constraint's reference version (e.g., a `gem::Version`)
/// - Type parameter `V` represents the version being validated (e.g., a `Revision`)
/// - Each ecosystem defines its own semantics for version validation rules
///
/// See the ecosystem-specific modules (pip, gem, nuget) for examples of how this trait
/// is implemented for each package manager's versioning rules.
pub trait Comparable<V> {
    /// Whether `self` is compatible with `V`.
    ///
    /// Each package ecosystem defines its own compatibility rules:
    /// - For pip: `~= 2.2` means `>= 2.2, < 3.0`
    /// - For gems: `~> 2.2.0` means `>= 2.2.0, < 2.3.0`
    /// - For semver: Similar to caret (`^`) dependency in Cargo
    fn compatible(&self, v: &V) -> bool;

    /// Whether `self` is exactly equal to `V`.
    fn equal(&self, v: &V) -> bool;

    /// Whether `self` is less than `V`.
    fn less(&self, v: &V) -> bool;

    /// Whether `self` is greater than `V`.
    fn greater(&self, v: &V) -> bool;

    /// Whether `self` is not equal to `V`.
    ///
    /// Default implementation uses the negation of `equal`.
    fn not_equal(&self, v: &V) -> bool {
        !self.equal(v)
    }

    /// Whether `self` is less than or equal to `V`.
    ///
    /// Default implementation combines `equal` and `less`.
    fn less_or_equal(&self, v: &V) -> bool {
        self.equal(v) || self.less(v)
    }

    /// Whether `self` is greater than or equal to `V`.
    ///
    /// Default implementation combines `equal` and `greater`.
    fn greater_or_equal(&self, v: &V) -> bool {
        self.equal(v) || self.greater(v)
    }
}

/// A generic version constraint that captures the type of comparison and target version.
///
/// `Constraint<V>` represents a single constraint (like "equal to 1.0.0" or "greater than 2.0.0")
/// where `V` is the type of version being constrained. This enables constraints to work with
/// different version types:
///
/// ```
/// # use locator::{Constraint, Revision};
/// # use semver::Version;
/// // A constraint on semver versions
/// let semver_constraint = Constraint::<Version>::Equal(Version::new(1, 0, 0));
///
/// // A constraint on opaque revisions
/// let revision_constraint = Constraint::<Revision>::GreaterOrEqual(Revision::from("1.0.0"));
/// ```
///
/// Each constraint type is evaluated according to the rules of the package ecosystem
/// through the `Comparable` trait implementation.
///
/// # Serialization
///
/// The constraint serialization is for transporting this type, not for parsing/serializing
/// the native format used by package managers.
///
/// # Comparison Rules
///
/// The exact semantics of each constraint type depend on the version type and its
/// `Comparable` implementation. See the ecosystem modules (`pip`, `gem`, `nuget`)
/// for specifics on how each ecosystem handles constraints.
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
    /// The comparing revision must be compatible with the provided revision.
    /// Note that the exact semantics of this constraint depend on the fetcher.
    #[assoc(revision = &_0)]
    #[new(into)]
    Compatible(V),

    /// The comparing revision must be exactly equal to the provided revision.
    #[assoc(revision = &_0)]
    #[new(into)]
    Equal(V),

    /// The comparing revision must not be exactly equal to the provided revision.
    #[assoc(revision = &_0)]
    #[new(into)]
    NotEqual(V),

    /// The comparing revision must be less than the provided revision.
    #[assoc(revision = &_0)]
    #[new(into)]
    Less(V),

    /// The comparing revision must be less than or equal to the provided revision.
    #[assoc(revision = &_0)]
    #[new(into)]
    LessOrEqual(V),

    /// The comparing revision must be greater than the provided revision.
    #[assoc(revision = &_0)]
    #[new(into)]
    Greater(V),

    /// The comparing revision must be greater than or equal to the provided revision.
    #[assoc(revision = &_0)]
    #[new(into)]
    GreaterOrEqual(V),
}

impl<V> Constraint<V> {
    /// Check if a version matches this constraint.
    ///
    /// The constraint's inner version (`V`) must implement `Comparable<T>` where
    /// `T` is the type of the version being checked. This enables constraints to validate
    /// against different version types.
    ///
    /// ```
    /// # use locator::{Constraint, Revision, constraint, revision};
    /// let constraint = constraint!(Equal => revision!(1, 0, 0));
    /// let version = Revision::from("1.0.0");
    ///
    /// assert!(constraint.matches(&version));
    /// ```
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

    /// Transform the interior value by reference, maintaining the constraint type.
    ///
    /// This is useful for adapting constraints between version types without cloning.
    ///
    /// ```
    /// # use locator::{Constraint, Revision, constraint, revision};
    /// # use std::fmt::Debug;
    /// // Convert a Revision constraint to a string-based constraint
    /// let rev_constraint = constraint!(Equal => revision!(1, 0, 0));
    /// let string_constraint = rev_constraint.map_ref(|r: &Revision| format!("{:?}", r));
    /// ```
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

    /// Transform the interior value by value, maintaining the constraint type.
    ///
    /// Similar to `map_ref` but consumes the constraint.
    ///
    /// ```
    /// # use locator::{Constraint, Revision, constraint, revision};
    /// # use semver::Version;
    /// // Convert a semver constraint to a revision constraint
    /// let sem_constraint = Constraint::<Version>::Equal(Version::new(1, 0, 0));
    /// let rev_constraint = sem_constraint.map(|v| Revision::Semver(v));
    /// ```
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
            Constraint::Compatible(v) => &v,
            Constraint::Equal(v) => &v,
            Constraint::NotEqual(v) => &v,
            Constraint::Less(v) => &v,
            Constraint::LessOrEqual(v) => &v,
            Constraint::Greater(v) => &v,
            Constraint::GreaterOrEqual(v) => &v,
        }
    }
}

/// A collection of version constraints that can be evaluated with AND/OR logic.
///
/// This type represents multiple constraints that can be applied to versions:
///
/// ```
/// # use locator::{Constraint, Constraints, Revision, constraint, revision, constraints};
/// // A range constraint: >= 1.0.0 AND < 2.0.0
/// let range = constraints!(
///     { GreaterOrEqual => revision!(1, 0, 0) },
///     { Less => revision!(2, 0, 0) }
/// );
///
/// // Version 1.5.0 satisfies both constraints
/// assert!(range.all_match(&Revision::from("1.5.0")));
///
/// // Version 2.5.0 fails the < 2.0.0 constraint
/// assert!(!range.all_match(&Revision::from("2.5.0")));
/// ```
///
/// Package managers like pip and gem often express version requirements as sets
/// of constraints combined with AND logic.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, Documented, ToSchema)]
#[non_exhaustive]
pub struct Constraints<V>(Vec<Constraint<V>>);

impl<V> Constraints<V> {
    /// Iterate over constraints in the set.
    pub fn iter(&self) -> impl Iterator<Item = &Constraint<V>> {
        self.0.iter()
    }

    /// Unpack into an iterator of constraints.
    pub fn into_iter(self) -> impl Iterator<Item = Constraint<V>> {
        self.0.into_iter()
    }

    /// Check if a version satisfies all constraints (AND logic).
    ///
    /// Returns true only if every constraint in the set is satisfied by the version.
    /// This is the most common evaluation logic for package version requirements.
    ///
    /// ```
    /// # use locator::{Constraints, Revision, constraints, revision};
    /// let range = constraints!(
    ///     { Greater => revision!(1, 0, 0) },
    ///     { Less => revision!(2, 0, 0) }
    /// );
    ///
    /// // Version 1.5.0 is both > 1.0.0 AND < 2.0.0
    /// assert!(range.all_match(&Revision::from("1.5.0")));
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

    /// Check if a version satisfies any constraint (OR logic).
    ///
    /// Returns true if at least one constraint in the set is satisfied by the version.
    /// This is useful for representing alternative version options.
    ///
    /// ```
    /// # use locator::{Constraints, Revision, constraints, revision};
    /// let options = constraints!(
    ///     { Equal => revision!(1, 0, 0) },
    ///     { Equal => revision!(2, 0, 0) }
    /// );
    ///
    /// // Either version 1.0.0 OR 2.0.0 is acceptable
    /// assert!(options.any_match(&Revision::from("1.0.0")));
    /// assert!(options.any_match(&Revision::from("2.0.0")));
    /// assert!(!options.any_match(&Revision::from("3.0.0")));
    /// ```
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
/// # use locator::{Constraint, Revision, semver::Version};
/// let constraint = locator::constraint!(Compatible => locator::revision!(1, 0, 0));
/// let expected = Constraint::Compatible(Revision::Semver(Version::new(1, 0, 0)));
/// assert_eq!(constraint, expected);
///
/// let constraint = locator::constraint!(Equal => locator::revision!("abcd1234"));
/// let expected = Constraint::Equal(Revision::Opaque(String::from("abcd1234")));
/// assert_eq!(constraint, expected);
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
/// # use locator::{Constraint, Constraints, Revision, semver::Version};
/// let constraint = locator::constraints!(
///     { Compatible => locator::revision!(1, 0, 0) },
///     { Compatible => locator::revision!(1, 1, 0) },
/// );
/// let expected = Constraints::from(vec![
///     Constraint::Compatible(Revision::Semver(Version::new(1, 0, 0))),
///     Constraint::Compatible(Revision::Semver(Version::new(1, 1, 0))),
/// ]);
/// assert_eq!(constraint, expected);
///
/// let constraint = locator::constraints!(
///     { Equal => locator::revision!("abcd1234") },
///     { Equal => locator::revision!("abcd12345") },
/// );
/// let expected = Constraints::from(vec![
///     Constraint::Equal(Revision::Opaque(String::from("abcd1234"))),
///     Constraint::Equal(Revision::Opaque(String::from("abcd12345"))),
/// ]);
/// assert_eq!(constraint, expected);
/// ```
#[macro_export]
macro_rules! constraints {
    ($({ $variant:ident => $($revision:tt)* }),* $(,)?) => {
        Constraints::from(vec![
            $(
                $crate::Constraint::$variant($($revision)*)
            ),*
        ])
    };
}

#[cfg(test)]
mod tests {
    use simple_test_case::test_case;

    use super::*;
    use crate::{Locator, Revision, StrictLocator, locator, revision, strict};

    #[test_case(constraint!(Compatible => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.4"); "1.2.4_compatible_1.2.3")]
    #[test_case(constraint!(Equal => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.3"); "1.2.3_equal_1.2.3")]
    #[test_case(constraint!(NotEqual => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.4"); "1.2.4_notequal_1.2.3")]
    #[test_case(constraint!(Less => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.2"); "1.2.2_less_1.2.3")]
    #[test_case(constraint!(LessOrEqual => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.2"); "1.2.2_less_or_equal_1.2.3")]
    #[test_case(constraint!(Greater => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.4"); "1.2.4_greater_1.2.3")]
    #[test_case(constraint!(GreaterOrEqual => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.4"); "1.2.4_greater_or_equal_1.2.3")]
    #[test]
    fn constraint_locator(constraint: Constraint<Revision>, target: Locator) {
        let revision = target.revision().as_ref().expect("must have a revision");
        assert!(
            constraint.matches(revision),
            "version '{target}' should match constraint '{constraint}'"
        );
    }

    #[test_case(constraint!(Compatible => revision!(1, 2, 3)), strict!(Archive, "pkg", "1.2.4"); "1.2.4_compatible_1.2.3")]
    #[test_case(constraint!(Equal => revision!(1, 2, 3)), strict!(Archive, "pkg", "1.2.3"); "1.2.3_equal_1.2.3")]
    #[test_case(constraint!(NotEqual => revision!(1, 2, 3)), strict!(Archive, "pkg", "1.2.4"); "1.2.4_notequal_1.2.3")]
    #[test_case(constraint!(Less => revision!(1, 2, 3)), strict!(Archive, "pkg", "1.2.2"); "1.2.2_less_1.2.3")]
    #[test_case(constraint!(LessOrEqual => revision!(1, 2, 3)), strict!(Archive, "pkg", "1.2.2"); "1.2.2_less_or_equal_1.2.3")]
    #[test_case(constraint!(Greater => revision!(1, 2, 3)), strict!(Archive, "pkg", "1.2.4"); "1.2.4_greater_1.2.3")]
    #[test_case(constraint!(GreaterOrEqual => revision!(1, 2, 3)), strict!(Archive, "pkg", "1.2.4"); "1.2.4_greater_or_equal_1.2.3")]
    #[test]
    fn constraint_strict_locator(constraint: Constraint<Revision>, target: StrictLocator) {
        assert!(
            constraint.matches(target.revision()),
            "version '{target}' should match constraint '{constraint}'"
        );
    }

    #[test_case(constraints!({ Compatible => revision!(2, 2, 3) }, { Compatible => revision!(1, 2, 3) }), locator!(Archive, "pkg", "1.2.4"); "1.2.4_compatible_1.2.3_or_2.2.3")]
    #[test_case(constraints!({ Equal => revision!("abcd") }, { Compatible => revision!("abcde") }), locator!(Archive, "pkg", "abcde"); "abcde_equal_abcd_or_compatible_abcde")]
    #[test]
    fn constraints_locator_any(constraints: Constraints<Revision>, target: Locator) {
        let revision = target.revision().as_ref().expect("must have a revision");
        assert!(
            constraints.any_match(revision),
            "version '{target}' should match at least one constraint in '{constraints:?}'"
        );
    }

    #[test_case(constraints!({ Greater => revision!(1, 2, 3) }, { Less => revision!(2, 0, 0) }), locator!(Archive, "pkg", "1.2.4"); "1.2.4_greater_1.2.3_and_less_2.0.0")]
    #[test_case(constraints!({ Less => revision!("abcd") }, { Greater => revision!("bbbb") }), locator!(Archive, "pkg", "abce"); "abce_greater_abcd_and_less_bbbb")]
    #[test]
    fn constraints_locator_all(constraints: Constraints<Revision>, target: Locator) {
        let revision = target.revision().as_ref().expect("must have a revision");
        assert!(
            constraints.all_match(revision),
            "version '{target}' should match all constraints in '{constraints:?}'"
        );
    }

    #[test_case(
        constraints!(
            { Compatible => revision!(2, 2, 3) },
            { Compatible => revision!(1, 2, 3) },
        ),
        strict!(Archive, "pkg", "1.2.4");
        "1.2.4_compatible_1.2.3_or_2.2.3"
    )]
    #[test_case(
        constraints!(
            { Equal => revision!("abcd") },
            { Compatible => revision!("abcde") },
        ),
        strict!(Archive, "pkg", "abcde");
        "abcde_equal_abcd_or_compatible_abcde"
    )]
    #[test]
    fn constraints_strict_locator_any(constraints: Constraints<Revision>, target: StrictLocator) {
        assert!(
            constraints.any_match(target.revision()),
            "version '{target}' should match at least one constraint in '{constraints:?}'"
        );
    }

    #[test_case(
        constraints!(
            { Greater => revision!(1, 2, 3) },
            { Less => revision!(2, 0, 0) },
        ),
        strict!(Archive, "pkg", "1.2.4");
        "1.2.4_greater_1.2.3_and_less_2.0.0"
    )]
    #[test_case(
        constraints!(
            { Less => revision!("abcd") },
            { Greater => revision!("bbbb") },
        ),
        strict!(Archive, "pkg", "abce");
        "abce_greater_abcd_and_less_bbbb"
    )]
    #[test]
    fn constraints_strict_locator_all(constraints: Constraints<Revision>, target: StrictLocator) {
        assert!(
            constraints.all_match(target.revision()),
            "version '{target}' should match all constraints in '{constraints:?}'"
        );
    }
}
