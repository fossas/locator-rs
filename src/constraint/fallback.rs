//! Implementation for fallback comparisons.
//!
//! # Revision to Revision
//!
//! Provides a fallback comparison for comparing [`Constraint<Revision>`] to [`Revision`].
//!
//! For such comparisons:
//! - If both versions are [`Revision::Semver`], compare according to semver rules.
//!   In this instance [`Constraint::Compatible`] is equivalent to a caret rule.
//! - If not, coerce them both to an [`Revision::Opaque`] and compare according to unicode ordering rules.
//!   In this instance [`Constraint::Compatible`] is a case-insensitive equality comparison.
//!
//! # Semver Version to Revision
//!
//! Provides a fallback comparison for comparing [`Constraint<semver::Version>`] to [`Revision`].
//!
//! For such comparisons:
//! - If the revision is [`Revision::Semver`], compare according to semver rules.
//!   In this instance [`Constraint::Compatible`] is equivalent to a caret rule.
//! - If the revision is [`Revision::Opaque`] the coerse the semver side to a string
//!   and compare according to unicode ordering rules.
//!   In this instance [`Constraint::Compatible`] is a case-insensitive equality comparison.
//!
//! # Semver Version to Semver Version
//!
//! Provides a fallback comparison for comparing [`Constraint<semver::Version>`] to `semver::Version`.
//!
//! Such comparisons are performed according to semver rules.
//! In this instance [`Constraint::Compatible`] is equivalent to a caret rule.
//!
//! # String to String
//!
//! Provides a fallback comparison for comparing [`Constraint<String>`] to `String`.
//!
//! Such comparisons are performed according to unicode ordering rules.
//! In this instance [`Constraint::Compatible`] is a case-insensitive equality comparison.

use std::cmp::Ordering;

use lexical_sort::lexical_cmp;
use semver::{Comparator, Op, Version};
use unicase::UniCase;

use crate::Revision;

use super::Comparable;

impl Comparable<Revision> for Revision {
    fn compatible(&self, rev: &Revision) -> bool {
        match (self, rev) {
            (Revision::Semver(s), Revision::Semver(r)) => s.compatible(r),
            (_, _) => self.to_string().compatible(&rev.to_string()),
        }
    }

    fn equal(&self, rev: &Revision) -> bool {
        match (self, rev) {
            (Revision::Semver(s), Revision::Semver(r)) => s.equal(r),
            (_, _) => self.to_string().equal(&rev.to_string()),
        }
    }

    fn less(&self, rev: &Revision) -> bool {
        match (self, rev) {
            (Revision::Semver(s), Revision::Semver(r)) => s.less(r),
            (_, _) => self.to_string().less(&rev.to_string()),
        }
    }

    fn greater(&self, rev: &Revision) -> bool {
        match (self, rev) {
            (Revision::Semver(s), Revision::Semver(r)) => s.greater(r),
            (_, _) => self.to_string().greater(&rev.to_string()),
        }
    }

    // The default trait impl for this would be kind of expensive since we have to `to_string` for `lexical_cmp`.
    fn less_or_equal(&self, rev: &Revision) -> bool {
        match (self, rev) {
            (Revision::Semver(s), Revision::Semver(r)) => s.less_or_equal(r),
            (_, _) => self.to_string().less_or_equal(&rev.to_string()),
        }
    }

    // The default trait impl for this would be kind of expensive since we have to `to_string` for `lexical_cmp`.
    fn greater_or_equal(&self, rev: &Revision) -> bool {
        match (self, rev) {
            (Revision::Semver(s), Revision::Semver(r)) => s.greater_or_equal(r),
            (_, _) => self.to_string().greater_or_equal(&rev.to_string()),
        }
    }
}

impl Comparable<Version> for Revision {
    fn compatible(&self, rev: &Version) -> bool {
        match self {
            Revision::Semver(s) => s.compatible(rev),
            Revision::Opaque(s) => s.to_string().compatible(&rev.to_string()),
        }
    }

    fn equal(&self, rev: &Version) -> bool {
        match self {
            Revision::Semver(s) => s.equal(rev),
            Revision::Opaque(s) => s.to_string().equal(&rev.to_string()),
        }
    }

    fn less(&self, rev: &Version) -> bool {
        match self {
            Revision::Semver(s) => s.less(rev),
            Revision::Opaque(s) => s.to_string().less(&rev.to_string()),
        }
    }

    fn greater(&self, rev: &Version) -> bool {
        match self {
            Revision::Semver(s) => s.greater(rev),
            Revision::Opaque(s) => s.to_string().greater(&rev.to_string()),
        }
    }

    fn less_or_equal(&self, rev: &Version) -> bool {
        match self {
            Revision::Semver(s) => s.less_or_equal(rev),
            Revision::Opaque(s) => s.to_string().less_or_equal(&rev.to_string()),
        }
    }

    fn greater_or_equal(&self, rev: &Version) -> bool {
        match self {
            Revision::Semver(s) => s.greater_or_equal(rev),
            Revision::Opaque(s) => s.to_string().greater_or_equal(&rev.to_string()),
        }
    }
}

impl Comparable<Version> for Version {
    fn compatible(&self, rev: &Version) -> bool {
        comparator(Op::Tilde, self).matches(rev)
    }

    fn equal(&self, rev: &Version) -> bool {
        comparator(Op::Exact, self).matches(rev)
    }

    fn less(&self, rev: &Version) -> bool {
        comparator(Op::Less, self).matches(rev)
    }

    fn greater(&self, rev: &Version) -> bool {
        comparator(Op::Greater, self).matches(rev)
    }

    fn less_or_equal(&self, rev: &Version) -> bool {
        comparator(Op::LessEq, self).matches(rev)
    }

    fn greater_or_equal(&self, rev: &Version) -> bool {
        comparator(Op::GreaterEq, self).matches(rev)
    }
}

impl Comparable<String> for String {
    fn compatible(&self, rev: &String) -> bool {
        UniCase::new(self) == UniCase::new(rev.to_string())
    }

    fn equal(&self, rev: &String) -> bool {
        lexical_cmp(self, &rev.to_string()) == Ordering::Equal
    }

    fn less(&self, rev: &String) -> bool {
        lexical_cmp(self, &rev.to_string()) == Ordering::Less
    }

    fn greater(&self, rev: &String) -> bool {
        lexical_cmp(self, &rev.to_string()) == Ordering::Greater
    }

    fn less_or_equal(&self, rev: &String) -> bool {
        lexical_cmp(self, &rev.to_string()) != Ordering::Greater
    }

    fn greater_or_equal(&self, rev: &String) -> bool {
        lexical_cmp(self, &rev.to_string()) != Ordering::Less
    }
}

fn comparator(op: Op, v: &Version) -> Comparator {
    Comparator {
        op,
        major: v.major,
        minor: Some(v.minor),
        patch: Some(v.patch),
        pre: v.pre.clone(),
    }
}

#[cfg(test)]
mod tests {
    use semver::Version;
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
            "check if version .{target}. matches constraint .{constraint}. to '{target}', expected: {expected}"
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
            "check if version .{target}. matches constraint .{constraint}. to '{target}', expected: {expected}"
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
            "check if version .{target}. matches constraint .{constraint}. to '{target}'"
        );
    }

    #[test_case(constraint!(Compatible => revision!(1, 0, 0)), Version::new(1, 0, 1); "1.0.0_compatible_1.0.1")]
    #[test_case(constraint!(Equal => revision!(1, 0, 0)), Version::new(1, 0, 0); "1.0.0_equal_1.0.0")]
    #[test_case(constraint!(NotEqual => revision!(1, 0, 0)), Version::new(1, 1, 0); "1.0.0_not_equal_1.1.0")]
    #[test_case(constraint!(Less => revision!(1, 0, 0)), Version::new(0, 9, 0); "1.0.0_less_0.9.0")]
    #[test_case(constraint!(Greater => revision!(1, 0, 0)), Version::new(1, 1, 0); "1.0.0_greater_1.1.0")]
    #[test_case(constraint!(LessOrEqual => revision!(1, 0, 0)), Version::new(1, 0, 0); "1.0.0_less_or_equal_1.0.0")]
    #[test_case(constraint!(GreaterOrEqual => revision!(1, 0, 0)), Version::new(1, 1, 0); "1.0.0_greater_or_equal_1.1.0")]
    #[test]
    fn compare_revision_to_semver_version(constraint: Constraint<Revision>, target: Version) {
        assert!(
            constraint.matches(&target),
            "check if version .{target}. matches constraint .{constraint}. to '{target}'"
        );
    }
}
