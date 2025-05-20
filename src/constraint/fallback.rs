//! # Default Constraint Comparison Implementations
//!
//! This module provides fallback comparison logic for version types that don't have
//! ecosystem-specific implementations. It enables the constraint system to work with
//! generic types while still providing sensible comparison behavior.
//!
//! ## Why Fallbacks Are Needed
//!
//! The fallback module serves several important purposes:
//!
//! 1. **Handling unknown version types**: Provides reasonable comparison behavior for custom
//!    or third-party version types
//! 2. **Type conversion**: Facilitates cross-type comparisons (e.g., semver to string)
//! 3. **Default behavior**: Establishes base rules that specific implementations can override
//! 4. **Consistency**: Ensures all constraint types have working implementations
//!
//! ## Implemented Comparisons
//!
//! ### Revision comparison
//!
//! - If both versions are [`Revision::Version`], use version comparison rules
//!   - [`Constraint::Compatible`] follows the caret (`^`) rule from Cargo
//! - If either version is [`Revision::Opaque`], convert both to strings and use lexical comparison
//!   - [`Constraint::Compatible`] performs a case-insensitive equality check
//!
//! ### Semver comparison
//!
//! - If the revision is able to be expressed as Semver, use strict semver rules
//! - Otherwise convert the semver to a string and use lexical comparison
//!
//! ### Version-shaped comparison
//!
//! - If the version isn't quite Semver, but appears version shaped, does the best it can
//! - Otherwise convert the version-shaped side to a string and use lexical comparison
//!
//! ### String to String
//!
//! - Uses lexical string comparison for ordered constraints (less, greater)
//! - [`Constraint::Compatible`] performs a case-insensitive equality check
//!
//! ## Implementation Details
//!
//! The fallback implementations use:
//! - `lexical_cmp` for string-based ordering (more intuitive than alphabetical)
//! - `UniCase` for case-insensitive matching in `Compatible` constraints
//! - `semver` compatible comparisons for strict semver checks when possible
//! - `versioning` compatible comparisons for version-shaped checks when possible

use std::cmp::Ordering;

use lexical_sort::lexical_cmp;
use unicase::UniCase;

use crate::{Revision, Version};

use super::Comparable;

impl Comparable<Revision> for Revision {
    fn compatible(&self, rev: &Revision) -> bool {
        match (self, rev) {
            (Revision::Version(s), Revision::Version(r)) => s.compatible(r),
            (_, _) => self.to_string().compatible(&rev.to_string()),
        }
    }

    fn equal(&self, rev: &Revision) -> bool {
        match (self, rev) {
            (Revision::Version(s), Revision::Version(r)) => s.equal(r),
            (_, _) => self.to_string().equal(&rev.to_string()),
        }
    }

    fn less(&self, rev: &Revision) -> bool {
        match (self, rev) {
            (Revision::Version(s), Revision::Version(r)) => s.less(r),
            (_, _) => self.to_string().less(&rev.to_string()),
        }
    }

    fn greater(&self, rev: &Revision) -> bool {
        match (self, rev) {
            (Revision::Version(s), Revision::Version(r)) => s.greater(r),
            (_, _) => self.to_string().greater(&rev.to_string()),
        }
    }

    // The default trait impl for this would be kind of expensive since we have to `to_string` for `lexical_cmp`.
    fn less_or_equal(&self, rev: &Revision) -> bool {
        match (self, rev) {
            (Revision::Version(s), Revision::Version(r)) => s.less_or_equal(r),
            (_, _) => self.to_string().less_or_equal(&rev.to_string()),
        }
    }

    // The default trait impl for this would be kind of expensive since we have to `to_string` for `lexical_cmp`.
    fn greater_or_equal(&self, rev: &Revision) -> bool {
        match (self, rev) {
            (Revision::Version(s), Revision::Version(r)) => s.greater_or_equal(r),
            (_, _) => self.to_string().greater_or_equal(&rev.to_string()),
        }
    }
}

impl Comparable<semver::Version> for Revision {
    fn compatible(&self, rev: &semver::Version) -> bool {
        match self {
            Revision::Version(s) => s.compatible(rev),
            Revision::Opaque(s) => s.to_string().compatible(&rev.to_string()),
        }
    }

    fn equal(&self, rev: &semver::Version) -> bool {
        match self {
            Revision::Version(s) => s.equal(rev),
            Revision::Opaque(s) => s.to_string().compatible(&rev.to_string()),
        }
    }

    fn less(&self, rev: &semver::Version) -> bool {
        match self {
            Revision::Version(s) => s.less(rev),
            Revision::Opaque(s) => s.to_string().compatible(&rev.to_string()),
        }
    }

    fn greater(&self, rev: &semver::Version) -> bool {
        match self {
            Revision::Version(s) => s.greater(rev),
            Revision::Opaque(s) => s.to_string().compatible(&rev.to_string()),
        }
    }

    fn less_or_equal(&self, rev: &semver::Version) -> bool {
        match self {
            Revision::Version(s) => s.less_or_equal(rev),
            Revision::Opaque(s) => s.to_string().compatible(&rev.to_string()),
        }
    }

    fn greater_or_equal(&self, rev: &semver::Version) -> bool {
        match self {
            Revision::Version(s) => s.greater_or_equal(rev),
            Revision::Opaque(s) => s.to_string().compatible(&rev.to_string()),
        }
    }
}

impl Comparable<Version> for Revision {
    fn compatible(&self, rev: &Version) -> bool {
        match self {
            Revision::Version(s) => s.compatible(rev),
            Revision::Opaque(s) => s.to_string().compatible(&rev.to_string()),
        }
    }

    fn equal(&self, rev: &Version) -> bool {
        match self {
            Revision::Version(s) => s.equal(rev),
            Revision::Opaque(s) => s.to_string().equal(&rev.to_string()),
        }
    }

    fn less(&self, rev: &Version) -> bool {
        match self {
            Revision::Version(s) => s.less(rev),
            Revision::Opaque(s) => s.to_string().less(&rev.to_string()),
        }
    }

    fn greater(&self, rev: &Version) -> bool {
        match self {
            Revision::Version(s) => s.greater(rev),
            Revision::Opaque(s) => s.to_string().greater(&rev.to_string()),
        }
    }

    fn less_or_equal(&self, rev: &Version) -> bool {
        match self {
            Revision::Version(s) => s.less_or_equal(rev),
            Revision::Opaque(s) => s.to_string().less_or_equal(&rev.to_string()),
        }
    }

    fn greater_or_equal(&self, rev: &Version) -> bool {
        match self {
            Revision::Version(s) => s.greater_or_equal(rev),
            Revision::Opaque(s) => s.to_string().greater_or_equal(&rev.to_string()),
        }
    }
}

impl Comparable<Version> for Version {
    fn compatible(&self, v: &Version) -> bool {
        self.parsed.compatible(&v.parsed)
    }

    fn equal(&self, v: &Version) -> bool {
        self.parsed.equal(&v.parsed)
    }

    fn less(&self, v: &Version) -> bool {
        self.parsed.less(&v.parsed)
    }

    fn greater(&self, v: &Version) -> bool {
        self.parsed.greater(&v.parsed)
    }

    fn less_or_equal(&self, v: &Version) -> bool {
        self.parsed.less_or_equal(&v.parsed)
    }

    fn greater_or_equal(&self, v: &Version) -> bool {
        self.parsed.greater_or_equal(&v.parsed)
    }
}

impl Comparable<versions::Versioning> for versions::Versioning {
    fn compatible(&self, rev: &versions::Versioning) -> bool {
        requirement(versions::Op::Caret, self).matches(rev)
    }

    fn equal(&self, rev: &versions::Versioning) -> bool {
        requirement(versions::Op::Exact, self).matches(rev)
    }

    fn less(&self, rev: &versions::Versioning) -> bool {
        requirement(versions::Op::Less, self).matches(rev)
    }

    fn greater(&self, rev: &versions::Versioning) -> bool {
        requirement(versions::Op::Greater, self).matches(rev)
    }

    fn less_or_equal(&self, rev: &versions::Versioning) -> bool {
        requirement(versions::Op::LessEq, self).matches(rev)
    }

    fn greater_or_equal(&self, rev: &versions::Versioning) -> bool {
        requirement(versions::Op::GreaterEq, self).matches(rev)
    }
}

impl Comparable<semver::Version> for Version {
    fn compatible(&self, v: &semver::Version) -> bool {
        self.parsed.compatible(v)
    }

    fn equal(&self, v: &semver::Version) -> bool {
        self.parsed.equal(v)
    }

    fn less(&self, v: &semver::Version) -> bool {
        self.parsed.less(v)
    }

    fn greater(&self, v: &semver::Version) -> bool {
        self.parsed.greater(v)
    }

    fn less_or_equal(&self, v: &semver::Version) -> bool {
        self.parsed.less_or_equal(v)
    }

    fn greater_or_equal(&self, v: &semver::Version) -> bool {
        self.parsed.greater_or_equal(v)
    }
}

impl Comparable<semver::Version> for versions::Versioning {
    fn compatible(&self, rev: &semver::Version) -> bool {
        match versions::Versioning::new(rev.to_string()) {
            Some(rev) => requirement(versions::Op::Caret, self).matches(&rev),
            None => false,
        }
    }

    fn equal(&self, rev: &semver::Version) -> bool {
        match versions::Versioning::new(rev.to_string()) {
            Some(rev) => requirement(versions::Op::Exact, self).matches(&rev),
            None => false,
        }
    }

    fn less(&self, rev: &semver::Version) -> bool {
        match versions::Versioning::new(rev.to_string()) {
            Some(rev) => requirement(versions::Op::Less, self).matches(&rev),
            None => false,
        }
    }

    fn greater(&self, rev: &semver::Version) -> bool {
        match versions::Versioning::new(rev.to_string()) {
            Some(rev) => requirement(versions::Op::Greater, self).matches(&rev),
            None => false,
        }
    }

    fn less_or_equal(&self, rev: &semver::Version) -> bool {
        match versions::Versioning::new(rev.to_string()) {
            Some(rev) => requirement(versions::Op::LessEq, self).matches(&rev),
            None => false,
        }
    }

    fn greater_or_equal(&self, rev: &semver::Version) -> bool {
        match versions::Versioning::new(rev.to_string()) {
            Some(rev) => requirement(versions::Op::GreaterEq, self).matches(&rev),
            None => false,
        }
    }
}

impl Comparable<semver::Version> for semver::Version {
    fn compatible(&self, rev: &semver::Version) -> bool {
        comparator(semver::Op::Caret, self).matches(rev)
    }

    fn equal(&self, rev: &semver::Version) -> bool {
        comparator(semver::Op::Exact, self).matches(rev)
    }

    fn less(&self, rev: &semver::Version) -> bool {
        comparator(semver::Op::Less, self).matches(rev)
    }

    fn greater(&self, rev: &semver::Version) -> bool {
        comparator(semver::Op::Greater, self).matches(rev)
    }

    fn less_or_equal(&self, rev: &semver::Version) -> bool {
        comparator(semver::Op::LessEq, self).matches(rev)
    }

    fn greater_or_equal(&self, rev: &semver::Version) -> bool {
        comparator(semver::Op::GreaterEq, self).matches(rev)
    }
}

impl<S: AsRef<str>> Comparable<S> for String {
    fn compatible(&self, rev: &S) -> bool {
        UniCase::new(self.as_str()) == UniCase::new(rev)
    }

    fn equal(&self, rev: &S) -> bool {
        lexical_cmp(self.as_str(), rev.as_ref()) == Ordering::Equal
    }

    fn less(&self, rev: &S) -> bool {
        lexical_cmp(self.as_str(), rev.as_ref()) == Ordering::Less
    }

    fn greater(&self, rev: &S) -> bool {
        lexical_cmp(self.as_str(), rev.as_ref()) == Ordering::Greater
    }

    fn less_or_equal(&self, rev: &S) -> bool {
        lexical_cmp(self.as_str(), rev.as_ref()) != Ordering::Greater
    }

    fn greater_or_equal(&self, rev: &S) -> bool {
        lexical_cmp(self.as_str(), rev.as_ref()) != Ordering::Less
    }
}

fn requirement(op: versions::Op, v: &versions::Versioning) -> versions::Requirement {
    match op {
        versions::Op::Wildcard => versions::Requirement { op, version: None },
        _ => versions::Requirement {
            op,
            version: Some(v.clone()),
        },
    }
}

fn comparator(op: semver::Op, v: &semver::Version) -> semver::Comparator {
    semver::Comparator {
        op,
        major: v.major,
        minor: Some(v.minor),
        patch: Some(v.patch),
        pre: v.pre.clone(),
    }
}

#[cfg(test)]
mod tests {
    use semver::Version as SemVer;
    use simple_test_case::test_case;

    use crate::{Constraint, Revision, constraint, revision};

    #[test_case(constraint!(Compatible => revision!(1, 2, 3)), Revision::from("1.2.3"), true; "1.2.3_compatible_1.2.3")]
    #[test_case(constraint!(Compatible => revision!(1, 2, 3)), Revision::from("1.2.4"), true; "1.2.4_compatible_1.2.3")]
    #[test_case(constraint!(Compatible => revision!(1, 2, 3)), Revision::from("2.0.0"), false; "2.0.0_not_compatible_1.2.3")]
    #[test_case(constraint!(Equal => revision!(1, 2, 3)), Revision::from("1.2.3"), true; "1.2.3_equal_1.2.3")]
    #[test_case(constraint!(Equal => revision!(1, 2, 3)), Revision::from("1.2.4"), false; "1.2.4_not_equal_1.2.3")]
    #[test_case(constraint!(NotEqual => revision!(1, 2, 3)), Revision::from("1.2.3"), false; "1.2.3_not_notequal_1.2.3")]
    #[test_case(constraint!(NotEqual => revision!(1, 2, 3)), Revision::from("1.2.4"), true; "1.2.4_notequal_1.2.3")]
    #[test_case(constraint!(Less => revision!(1, 2, 3)), Revision::from("1.2.2"), true; "1.2.2_less_1.2.3")]
    #[test_case(constraint!(Less => revision!(1, 2, 3)), Revision::from("1.2.3"), false; "1.2.3_not_less_1.2.3")]
    #[test_case(constraint!(LessOrEqual => revision!(1, 2, 3)), Revision::from("1.2.2"), true; "1.2.2_less_or_equal_1.2.3")]
    #[test_case(constraint!(LessOrEqual => revision!(1, 2, 3)), Revision::from("1.2.3"), true; "1.2.3_less_or_equal_1.2.3")]
    #[test_case(constraint!(LessOrEqual => revision!(1, 2, 3)), Revision::from("1.2.4"), false; "1.2.4_not_less_or_equal_1.2.3")]
    #[test_case(constraint!(Greater => revision!(1, 2, 3)), Revision::from("1.2.4"), true; "1.2.4_greater_1.2.3")]
    #[test_case(constraint!(Greater => revision!(1, 2, 3)), Revision::from("1.2.3"), false; "1.2.3_not_greater_1.2.3")]
    #[test_case(constraint!(GreaterOrEqual => revision!(1, 2, 3)), Revision::from("1.2.4"), true; "1.2.4_greater_or_equal_1.2.3")]
    #[test_case(constraint!(GreaterOrEqual => revision!(1, 2, 3)), Revision::from("1.2.3"), true; "1.2.3_greater_or_equal_1.2.3")]
    #[test_case(constraint!(GreaterOrEqual => revision!(1, 2, 3)), Revision::from("1.2.2"), false; "1.2.2_not_greater_or_equal_1.2.3")]
    #[test]
    fn compare_revisions_semver(
        constraint: Constraint<Revision>,
        target: Revision,
        expected: bool,
    ) {
        assert_eq!(
            constraint.matches(&target),
            expected,
            "check if version '{target:?}' matches constraint '{constraint:?}, expected: {expected}"
        );
    }

    #[test_case(constraint!(Compatible => revision!("abcd")), Revision::from("AbCd"), true; "abcd_compatible_AbCd")]
    #[test_case(constraint!(Compatible => revision!("abcd")), Revision::from("AbCdE"), false; "abcd_not_compatible_AbCdE")]
    #[test_case(constraint!(Equal => revision!("abcd")), Revision::from("abcd"), true; "abcd_equal_abcd")]
    #[test_case(constraint!(Equal => revision!("abcd")), Revision::from("aBcD"), false; "abcd_not_equal_aBcD")]
    #[test_case(constraint!(NotEqual => revision!("abcd")), Revision::from("abcde"), true; "abcd_notequal_abcde")]
    #[test_case(constraint!(NotEqual => revision!("abcd")), Revision::from("abcd"), false; "abcd_not_notequal_abcd")]
    #[test_case(constraint!(Less => revision!("a")), Revision::from("b"), true; "a_less_b")]
    #[test_case(constraint!(Less => revision!("a")), Revision::from("a"), false; "a_not_less_a")]
    #[test_case(constraint!(Greater => revision!("b")), Revision::from("a"), true; "b_greater_a")]
    #[test_case(constraint!(Greater => revision!("b")), Revision::from("c"), false; "b_not_greater_c")]
    #[test_case(constraint!(Less => revision!("あ")), Revision::from("え"), true; "jp_a_less_e")]
    #[test_case(constraint!(Greater => revision!("え")), Revision::from("あ"), true; "jp_e_greater_a")]
    #[test_case(constraint!(Equal => revision!("あ")), Revision::from("あ"), true; "jp_a_equal_a")]
    #[test_case(constraint!(Compatible => revision!("Maße")), Revision::from("MASSE"), true; "gr_masse_compatible_MASSE")]
    #[test]
    fn compare_revisions_opaque(
        constraint: Constraint<Revision>,
        target: Revision,
        expected: bool,
    ) {
        assert_eq!(
            constraint.matches(&target),
            expected,
            "check if version '{target:?}' matches constraint '{constraint:?}', expected: {expected}"
        );
    }

    #[test_case(constraint!(Compatible => String::from("abc")), String::from("ABC"); "abc_compatible_ABC")]
    #[test_case(constraint!(Equal => String::from("abc")), String::from("abc"); "abc_equal_abc")]
    #[test_case(constraint!(NotEqual => String::from("abc")), String::from("ABC"); "abc_not_equal_ABC")]
    #[test_case(constraint!(Less => String::from("abc1")), String::from("abc2"); "abc_less_abc2")]
    #[test_case(constraint!(Less => String::from("a")), String::from("b"); "a_less_ab")]
    #[test_case(constraint!(Greater => String::from("abc2")), String::from("abc1"); "abc_greater_abc1")]
    #[test_case(constraint!(LessOrEqual => String::from("abc1")), String::from("abc2"); "abc_less_or_equal_abc2")]
    #[test_case(constraint!(GreaterOrEqual => String::from("abc2")), String::from("abc1"); "abc_greater_or_equal_abc1")]
    #[test]
    fn compare_strings(constraint: Constraint<String>, target: String) {
        assert!(
            constraint.matches(&target),
            "check if version '{target:?}' matches constraint '{constraint:?}"
        );
    }

    #[test_case(constraint!(Compatible => revision!(1, 0, 0)), SemVer::new(1, 0, 1); "1.0.0_compatible_1.0.1")]
    #[test_case(constraint!(Equal => revision!(1, 0, 0)), SemVer::new(1, 0, 0); "1.0.0_equal_1.0.0")]
    #[test_case(constraint!(NotEqual => revision!(1, 0, 0)), SemVer::new(1, 1, 0); "1.0.0_not_equal_1.1.0")]
    #[test_case(constraint!(Less => revision!(1, 0, 0)), SemVer::new(0, 9, 0); "1.0.0_less_0.9.0")]
    #[test_case(constraint!(Greater => revision!(1, 0, 0)), SemVer::new(1, 1, 0); "1.0.0_greater_1.1.0")]
    #[test_case(constraint!(LessOrEqual => revision!(1, 0, 0)), SemVer::new(1, 0, 0); "1.0.0_less_or_equal_1.0.0")]
    #[test_case(constraint!(GreaterOrEqual => revision!(1, 0, 0)), SemVer::new(1, 1, 0); "1.0.0_greater_or_equal_1.1.0")]
    #[test]
    fn compare_revision_to_semver_version(constraint: Constraint<Revision>, target: SemVer) {
        assert!(
            constraint.matches(&target),
            "check if version '{target:?}' matches constraint '{constraint:?}"
        );
    }
}
