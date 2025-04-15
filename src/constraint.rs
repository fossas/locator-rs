use derive_new::new;
use documented::Documented;
use enum_assoc::Assoc;
use serde::{Deserialize, Serialize};
use strum::Display;
use tracing::warn;
use utoipa::ToSchema;

use crate::{Fetcher, Revision};

mod fallback;
mod gem;

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
    new,
)]
#[schema(example = json!({ "kind": "equal", "value": "1.0.0"}))]
#[serde(rename_all = "snake_case", tag = "kind", content = "value")]
#[func(const fn revision(&self) -> &Revision)]
#[non_exhaustive]
pub enum Constraint {
    /// The comparing revision must be compatible with the provided revision.
    /// Note that the exact semantics of this constraint depend on the fetcher.
    #[strum(to_string = "~={0:?}")]
    #[assoc(revision = &_0)]
    #[new(into)]
    Compatible(Revision),

    /// The comparing revision must be exactly equal to the provided revision.
    #[strum(to_string = "=={0:?}")]
    #[assoc(revision = &_0)]
    #[new(into)]
    Equal(Revision),

    /// The comparing revision must not be exactly equal to the provided revision.
    #[strum(to_string = "!={0:?}")]
    #[assoc(revision = &_0)]
    #[new(into)]
    NotEqual(Revision),

    /// The comparing revision must be less than the provided revision.
    #[strum(to_string = "<{0:?}")]
    #[assoc(revision = &_0)]
    #[new(into)]
    Less(Revision),

    /// The comparing revision must be less than or equal to the provided revision.
    #[strum(to_string = "<={0:?}")]
    #[assoc(revision = &_0)]
    #[new(into)]
    LessOrEqual(Revision),

    /// The comparing revision must be greater than the provided revision.
    #[strum(to_string = ">{0:?}")]
    #[assoc(revision = &_0)]
    #[new(into)]
    Greater(Revision),

    /// The comparing revision must be greater than or equal to the provided revision.
    #[strum(to_string = ">={0:?}")]
    #[assoc(revision = &_0)]
    #[new(into)]
    GreaterOrEqual(Revision),
}

impl Constraint {
    /// Compare the constraint to the target revision according to the rules of the provided fetcher.
    ///
    /// The default if there are no additional rules specified for the fetcher is:
    /// - If both versions are semver, compare according to semver rules.
    /// - If not, coerce them both to an opaque string and compare according to unicode ordering rules.
    ///   In this instance [`Constraint::Compatible`] is a case-insensitive equality comparison.
    pub fn compare(&self, fetcher: Fetcher, target: &Revision) -> bool {
        match fetcher {
            // If no specific comparitor is configured for this fetcher,
            // compare using the generic fallback.
            Fetcher::Gem => gem::compare(self, Fetcher::Gem, target).unwrap_or_else(|err| {
                warn!("could not parse rubygems version '{err:?}'");
                fallback::compare(self, Fetcher::Gem, target)
            }),
            other => fallback::compare(self, other, target),
        }
    }
}

impl From<&Constraint> for Constraint {
    fn from(c: &Constraint) -> Self {
        c.clone()
    }
}

impl AsRef<Constraint> for Constraint {
    fn as_ref(&self) -> &Constraint {
        self
    }
}

/// A set of [`Constraint`].
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, Documented, ToSchema)]
#[non_exhaustive]
pub struct Constraints(Vec<Constraint>);

impl Constraints {
    /// Iterate over constraints in the set.
    pub fn iter(&self) -> impl Iterator<Item = &Constraint> {
        self.0.iter()
    }

    /// Compare the constraints to the target revision according to the rules of the provided fetcher.
    ///
    /// This method compares in an `AND` fashion: it only returns true if _all_ constraints
    /// inside of this set compare favorably.
    pub fn compare_all(&self, fetcher: Fetcher, target: &Revision) -> bool {
        for constraint in self.iter() {
            if !constraint.compare(fetcher, target) {
                return false;
            }
        }
        true
    }

    /// Compare the constraints to the target revision according to the rules of the provided fetcher.
    ///
    /// This method compares in an `OR` fashion: it returns true if _any_ constraint
    /// inside of this set compare favorably.
    pub fn compare_any(&self, fetcher: Fetcher, target: &Revision) -> bool {
        for constraint in self.iter() {
            if constraint.compare(fetcher, target) {
                return true;
            }
        }
        false
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

impl From<Constraint> for Constraints {
    fn from(constraint: Constraint) -> Self {
        Self(vec![constraint])
    }
}

impl From<&Constraint> for Constraints {
    fn from(constraint: &Constraint) -> Self {
        constraint.clone().into()
    }
}

impl AsRef<Constraints> for Constraints {
    fn as_ref(&self) -> &Constraints {
        self
    }
}

impl crate::Locator {
    /// Compare the target constraints to this locator's revision, according to the rules of its fetcher.
    ///
    /// This method compares in an `AND` fashion: it only returns true if _all_ constraints
    /// inside of this set compare favorably.
    ///
    /// If the locator does not have a revision component, this comparison automaticlly fails;
    /// it's assumed that if there are constraints of any kind, they can't possibly be validated.
    pub fn compare_all(&self, constraints: impl AsRef<Constraints>) -> bool {
        match self.revision().as_ref() {
            Some(target) => constraints.as_ref().compare_all(self.fetcher(), target),
            None => false,
        }
    }

    /// Compare the constraints to the target revision according to the rules of the provided fetcher.
    ///
    /// This method compares in an `OR` fashion: it returns true if _any_ constraint
    /// inside of this set compare favorably.
    pub fn compare_any(&self, constraints: impl AsRef<Constraints>) -> bool {
        match self.revision().as_ref() {
            Some(target) => constraints.as_ref().compare_any(self.fetcher(), target),
            None => false,
        }
    }

    /// Compare the constraint to the target revision according to the rules of the provided fetcher.
    pub fn compare(&self, constraint: impl AsRef<Constraint>) -> bool {
        match self.revision().as_ref() {
            Some(target) => constraint.as_ref().compare(self.fetcher(), target),
            None => false,
        }
    }
}

impl crate::StrictLocator {
    /// Compare the target constraints to this locator's revision, according to the rules of its fetcher.
    ///
    /// This method compares in an `AND` fashion: it only returns true if _all_ constraints
    /// inside of this set compare favorably.
    pub fn compare_all(&self, constraints: impl AsRef<Constraints>) -> bool {
        let (fetcher, revision) = (self.fetcher(), self.revision());
        constraints.as_ref().compare_all(fetcher, revision)
    }

    /// Compare the constraints to the target revision according to the rules of the provided fetcher.
    ///
    /// This method compares in an `OR` fashion: it returns true if _any_ constraint
    /// inside of this set compare favorably.
    pub fn compare_any(&self, constraints: impl AsRef<Constraints>) -> bool {
        let (fetcher, revision) = (self.fetcher(), self.revision());
        constraints.as_ref().compare_any(fetcher, revision)
    }

    /// Compare the constraint to the target revision according to the rules of the provided fetcher.
    pub fn compare(&self, constraint: impl AsRef<Constraint>) -> bool {
        let (fetcher, revision) = (self.fetcher(), self.revision());
        constraint.as_ref().compare(fetcher, revision)
    }
}

/// Construct a [`Constraint`], guaranteed to be valid at compile time.
///
/// ```
/// # use locator::{Constraint, Revision, semver::Version};
/// let constraint = locator::constraint!(Compatible => 1, 0, 0);
/// let expected = Constraint::Compatible(Revision::Semver(Version::new(1, 0, 0)));
/// assert_eq!(constraint, expected);
///
/// let constraint = locator::constraint!(Equal => "abcd1234");
/// let expected = Constraint::Equal(Revision::Opaque(String::from("abcd1234")));
/// assert_eq!(constraint, expected);
/// ```
#[macro_export]
macro_rules! constraint {
    ($variant:ident => $major:literal, $minor:literal, $patch:literal) => {
        $crate::Constraint::$variant($crate::Revision::Semver($crate::semver::Version::new(
            $major, $minor, $patch,
        )))
    };
    ($variant:ident => $opaque:literal) => {
        $crate::Constraint::$variant($crate::Revision::Opaque($opaque.into()))
    };
}

/// Construct multiple [`Constraints`], guaranteed to be valid at compile time.
///
/// ```
/// # use locator::{Constraint, Constraints, Revision, semver::Version};
/// let constraint = locator::constraints!(
///     Compatible => 1, 0, 0;
///     Compatible => 1, 1, 0;
/// );
/// let expected = Constraints::from(vec![
///     Constraint::Compatible(Revision::Semver(Version::new(1, 0, 0))),
///     Constraint::Compatible(Revision::Semver(Version::new(1, 1, 0))),
/// ]);
/// assert_eq!(constraint, expected);
///
/// let constraint = locator::constraints!(
///     Equal => "abcd1234";
///     Equal => "abcd12345";
/// );
/// let expected = Constraints::from(vec![
///     Constraint::Equal(Revision::Opaque(String::from("abcd1234"))),
///     Constraint::Equal(Revision::Opaque(String::from("abcd12345"))),
/// ]);
/// assert_eq!(constraint, expected);
/// ```
#[macro_export]
macro_rules! constraints {
    ($($variant:ident => $major:literal, $minor:literal, $patch:literal);* $(;)?) => {
        Constraints::from(vec![
            $(
                $crate::Constraint::$variant($crate::Revision::Semver($crate::semver::Version::new(
                    $major, $minor, $patch,
                )))
            ),*
        ])
    };
    ($($variant:ident => $opaque:literal);* $(;)?) => {
        Constraints::from(vec![
            $(
                $crate::Constraint::$variant($crate::Revision::Opaque($opaque.into()))
            ),*
        ])
    };
}

#[cfg(test)]
mod tests {
    use simple_test_case::test_case;

    use super::*;
    use crate::{Locator, StrictLocator, locator, strict};

    // Tests in this module use a fetcher that is unlikely to ever have actual comparison functionality
    // so that it uses the fallback. The tests here are mostly meant to test that the fetcher
    // actually uses the comparison method, not so much the comparison itself.

    #[test_case(constraint!(Compatible => 1, 2, 3), locator!(Archive, "pkg", "1.2.4"); "1.2.4_compatible_1.2.3")]
    #[test_case(constraint!(Equal => 1, 2, 3), locator!(Archive, "pkg", "1.2.3"); "1.2.3_equal_1.2.3")]
    #[test_case(constraint!(NotEqual => 1, 2, 3), locator!(Archive, "pkg", "1.2.4"); "1.2.4_notequal_1.2.3")]
    #[test_case(constraint!(Less => 1, 2, 3), locator!(Archive, "pkg", "1.2.2"); "1.2.2_less_1.2.3")]
    #[test_case(constraint!(LessOrEqual => 1, 2, 3), locator!(Archive, "pkg", "1.2.2"); "1.2.2_less_or_equal_1.2.3")]
    #[test_case(constraint!(Greater => 1, 2, 3), locator!(Archive, "pkg", "1.2.4"); "1.2.4_greater_1.2.3")]
    #[test_case(constraint!(GreaterOrEqual => 1, 2, 3), locator!(Archive, "pkg", "1.2.4"); "1.2.4_greater_or_equal_1.2.3")]
    #[test]
    fn constraint_locator(constraint: Constraint, target: Locator) {
        assert!(
            target.compare(&constraint),
            "compare '{target}' to '{constraint}'"
        );
    }

    #[test_case(constraint!(Compatible => 1, 2, 3), strict!(Archive, "pkg", "1.2.4"); "1.2.4_compatible_1.2.3")]
    #[test_case(constraint!(Equal => 1, 2, 3), strict!(Archive, "pkg", "1.2.3"); "1.2.3_equal_1.2.3")]
    #[test_case(constraint!(NotEqual => 1, 2, 3), strict!(Archive, "pkg", "1.2.4"); "1.2.4_notequal_1.2.3")]
    #[test_case(constraint!(Less => 1, 2, 3), strict!(Archive, "pkg", "1.2.2"); "1.2.2_less_1.2.3")]
    #[test_case(constraint!(LessOrEqual => 1, 2, 3), strict!(Archive, "pkg", "1.2.2"); "1.2.2_less_or_equal_1.2.3")]
    #[test_case(constraint!(Greater => 1, 2, 3), strict!(Archive, "pkg", "1.2.4"); "1.2.4_greater_1.2.3")]
    #[test_case(constraint!(GreaterOrEqual => 1, 2, 3), strict!(Archive, "pkg", "1.2.4"); "1.2.4_greater_or_equal_1.2.3")]
    #[test]
    fn constraint_strict_locator(constraint: Constraint, target: StrictLocator) {
        assert!(
            target.compare(&constraint),
            "compare '{target}' to '{constraint}'"
        );
    }

    #[test_case(constraints!(Compatible => 2, 2, 3; Compatible => 1, 2, 3), locator!(Archive, "pkg", "1.2.4"); "1.2.4_compatible_1.2.3_or_2.2.3")]
    #[test_case(constraints!(Equal => "abcd"; Compatible => "abcde"), locator!(Archive, "pkg", "abcde"); "abcde_equal_abcd_or_compatible_abcde")]
    #[test]
    fn constraints_locator_any(constraints: Constraints, target: Locator) {
        assert!(
            target.compare_any(&constraints),
            "compare '{target}' to '{constraints:?}'"
        );
    }

    #[test_case(constraints!(Greater => 1, 2, 3; Less => 2, 0, 0), locator!(Archive, "pkg", "1.2.4"); "1.2.4_greater_1.2.3_and_less_2.0.0")]
    #[test_case(constraints!(Less => "abcd"; Greater => "bbbb"), locator!(Archive, "pkg", "abce"); "abce_greater_abcd_and_less_bbbb")]
    #[test]
    fn constraints_locator_all(constraints: Constraints, target: Locator) {
        assert!(
            target.compare_all(&constraints),
            "compare '{target}' to '{constraints:?}'"
        );
    }

    #[test_case(constraints!(Compatible => 2, 2, 3; Compatible => 1, 2, 3), strict!(Archive, "pkg", "1.2.4"); "1.2.4_compatible_1.2.3_or_2.2.3")]
    #[test_case(constraints!(Equal => "abcd"; Compatible => "abcde"), strict!(Archive, "pkg", "abcde"); "abcde_equal_abcd_or_compatible_abcde")]
    #[test]
    fn constraints_strict_locator_any(constraints: Constraints, target: StrictLocator) {
        assert!(
            target.compare_any(&constraints),
            "compare '{target}' to '{constraints:?}'"
        );
    }

    #[test_case(constraints!(Greater => 1, 2, 3; Less => 2, 0, 0), strict!(Archive, "pkg", "1.2.4"); "1.2.4_greater_1.2.3_and_less_2.0.0")]
    #[test_case(constraints!(Less => "abcd"; Greater => "bbbb"), strict!(Archive, "pkg", "abce"); "abce_greater_abcd_and_less_bbbb")]
    #[test]
    fn constraints_strict_locator_all(constraints: Constraints, target: StrictLocator) {
        assert!(
            target.compare_all(&constraints),
            "compare '{target}' to '{constraints:?}'"
        );
    }
}
