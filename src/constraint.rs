//! Version constraints and comparisons.

use derive_new::new;
use documented::Documented;
use enum_assoc::Assoc;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

pub mod fallback;
pub mod gem;
pub mod pip;
pub mod nuget;

/// Supports [`Constraint`] comparison operations generically with `V` for `Self`.
/// Comparable operations treat the constraint as the "left hand side".
///
/// Important: When _implementing_ `Comparable`, this means `self` _is the left-hand side_.
pub trait Comparable<V> {
    /// Whether `self` is compatible with `V`.
    fn compatible(&self, v: &V) -> bool;

    /// Whether `self` is equal to `V`.
    fn equal(&self, v: &V) -> bool;

    /// Whether `self` is less than `V`.
    fn less(&self, v: &V) -> bool;

    /// Whether `self` is greater than `V`.
    fn greater(&self, v: &V) -> bool;

    /// Whether `self` is not equal to `V`.
    fn not_equal(&self, v: &V) -> bool {
        !self.equal(v)
    }

    /// Whether `self` is less than or equal to `V`.
    fn less_or_equal(&self, v: &V) -> bool {
        self.equal(v) || self.less(v)
    }

    /// Whether `self` is greater than or equal to `V`.
    fn greater_or_equal(&self, v: &V) -> bool {
        self.equal(v) || self.greater(v)
    }
}

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

<<<<<<< HEAD
impl Constraint {
    /// Compare the constraint to the target revision according to the rules of the provided fetcher.
    ///
    /// The default if there are no additional rules specified for the fetcher is:
    /// - If both versions are semver, compare according to semver rules.
    /// - If not, coerce them both to an opaque string and compare according to unicode ordering rules.
    ///   In this instance [`Constraint::Compatible`] is a case-insensitive equality comparison.
    pub fn compare(&self, fetcher: Fetcher, target: &Revision) -> bool {
        match fetcher {
            Fetcher::Gem => gem::compare(self, Fetcher::Gem, target).unwrap_or_else(|err| {
                warn!(?err, "could not compare gem version");
                fallback::compare(self, Fetcher::Gem, target)
            }),
            Fetcher::Pip => pip::compare(self, Fetcher::Pip, target).unwrap_or_else(|err| {
                warn!(?err, "could not compare pip version");
                fallback::compare(self, Fetcher::Pip, target)
            }),
            Fetcher::Nuget => nuget::compare(self, Fetcher::Nuget, target).unwrap_or_else(|err| {
                warn!(?err, "could not compare nuget version");
                fallback::compare(self, Fetcher::Nuget, target)
            }),
            // If no specific comparitor is configured for this fetcher,
            // compare using the generic fallback.
            other => fallback::compare(self, other, target),
=======
impl<V> Constraint<V> {
    /// Compare the constraint to the target, with the constraint on the left-hand-side.
    pub fn compare<T>(&self, target: &T) -> bool
    where
        V: Comparable<T>,
    {
        match self {
            Constraint::Compatible(s) => s.compatible(target),
            Constraint::Equal(s) => s.equal(target),
            Constraint::NotEqual(s) => s.not_equal(target),
            Constraint::Less(s) => s.less(target),
            Constraint::LessOrEqual(s) => s.less_or_equal(target),
            Constraint::Greater(s) => s.greater(target),
            Constraint::GreaterOrEqual(s) => s.greater_or_equal(target),
        }
    }

    /// Transform the interior value by reference, keeping the constraint itself.
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

    /// Transform the interior value, keeping the constraint itself.
    pub fn map<R>(self, closure: impl Fn(V) -> R) -> Constraint<R> {
        match self {
            Constraint::Compatible(v) => Constraint::Compatible(closure(v)),
            Constraint::Equal(v) => Constraint::Equal(closure(v)),
            Constraint::NotEqual(v) => Constraint::NotEqual(closure(v)),
            Constraint::Less(v) => Constraint::Less(closure(v)),
            Constraint::LessOrEqual(v) => Constraint::LessOrEqual(closure(v)),
            Constraint::Greater(v) => Constraint::Greater(closure(v)),
            Constraint::GreaterOrEqual(v) => Constraint::GreaterOrEqual(closure(v)),
>>>>>>> 9d2847e (Refactor WIP)
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

/// A set of [`Constraint`].
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

    /// Compare the constraints to the target according to [`Constraint::compare`].
    ///
    /// This method compares in an `AND` fashion: it only returns true if _all_ constraints
    /// inside of this set compare favorably.
    pub fn compare_all<T>(&self, target: &T) -> bool
    where
        V: Comparable<T>,
    {
        for constraint in self.iter() {
            if !constraint.compare(target) {
                return false;
            }
        }
        true
    }

    /// Compare the constraints to the target according to [`Constraint::compare`].
    ///
    /// This method compares in an `OR` fashion: it returns true if _any_ constraint
    /// inside of this set compare favorably.
    pub fn compare_any<T>(&self, target: &T) -> bool
    where
        V: Comparable<T>,
    {
        for constraint in self.iter() {
            if constraint.compare(target) {
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
            constraint.compare(revision),
            "compare '{target}' to '{constraint}'"
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
            constraint.compare(target.revision()),
            "compare '{target}' to '{constraint}'"
        );
    }

    #[test_case(constraints!({ Compatible => revision!(2, 2, 3) }, { Compatible => revision!(1, 2, 3) }), locator!(Archive, "pkg", "1.2.4"); "1.2.4_compatible_1.2.3_or_2.2.3")]
    #[test_case(constraints!({ Equal => revision!("abcd") }, { Compatible => revision!("abcde") }), locator!(Archive, "pkg", "abcde"); "abcde_equal_abcd_or_compatible_abcde")]
    #[test]
    fn constraints_locator_any(constraints: Constraints<Revision>, target: Locator) {
        let revision = target.revision().as_ref().expect("must have a revision");
        assert!(
            constraints.compare_any(revision),
            "compare '{target}' to '{constraints:?}'"
        );
    }

    #[test_case(constraints!({ Greater => revision!(1, 2, 3) }, { Less => revision!(2, 0, 0) }), locator!(Archive, "pkg", "1.2.4"); "1.2.4_greater_1.2.3_and_less_2.0.0")]
    #[test_case(constraints!({ Less => revision!("abcd") }, { Greater => revision!("bbbb") }), locator!(Archive, "pkg", "abce"); "abce_greater_abcd_and_less_bbbb")]
    #[test]
    fn constraints_locator_all(constraints: Constraints<Revision>, target: Locator) {
        let revision = target.revision().as_ref().expect("must have a revision");
        assert!(
            constraints.compare_all(revision),
            "compare '{target}' to '{constraints:?}'"
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
            constraints.compare_any(target.revision()),
            "compare '{target}' to '{constraints:?}'"
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
            constraints.compare_all(target.revision()),
            "compare '{target}' to '{constraints:?}'"
        );
    }
}
