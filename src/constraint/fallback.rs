use std::cmp::Ordering;

use semver::{Comparator, Op, Version};
use unicase::UniCase;

use crate::{Fetcher, Revision};

use super::Constraint;

/// Fallback comparison when there is no fetcher-specific comparison.
///
/// The default if there are no additional rules specified for the fetcher is:
/// - If both versions are semver, compare according to semver rules.
///   In this instance [`Constraint::Compatible`] is equivalent to a caret rule.
/// - If not, coerce them both to an opaque string and compare according to unicode ordering rules.
///   In this instance [`Constraint::Compatible`] is a case-insensitive equality comparison.
pub fn compare(constraint: &Constraint, _: Fetcher, rev: &Revision) -> bool {
    if let (Revision::Semver(c), Revision::Semver(r)) = (constraint.revision(), rev) {
        return match constraint {
            Constraint::Compatible(_) => comparator(Op::Tilde, c).matches(r),
            Constraint::Equal(_) => comparator(Op::Exact, c).matches(r),
            Constraint::NotEqual(_) => !comparator(Op::Exact, c).matches(r),
            Constraint::Less(_) => comparator(Op::Less, c).matches(r),
            Constraint::LessOrEqual(_) => comparator(Op::LessEq, c).matches(r),
            Constraint::Greater(_) => comparator(Op::Greater, c).matches(r),
            Constraint::GreaterOrEqual(_) => comparator(Op::GreaterEq, c).matches(r),
        };
    }

    match constraint {
        Constraint::Compatible(c) => UniCase::new(c.as_str()) == UniCase::new(rev.as_str()),
        _ => lexically_compare(constraint, rev),
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

fn ords_for(constraint: &Constraint) -> Vec<Ordering> {
    match constraint {
        Constraint::Compatible(_) => vec![Ordering::Equal],
        Constraint::Equal(_) => vec![Ordering::Equal],
        Constraint::NotEqual(_) => vec![Ordering::Less, Ordering::Greater],
        Constraint::Less(_) => vec![Ordering::Less],
        Constraint::LessOrEqual(_) => vec![Ordering::Less, Ordering::Equal],
        Constraint::Greater(_) => vec![Ordering::Greater],
        Constraint::GreaterOrEqual(_) => vec![Ordering::Greater, Ordering::Equal],
    }
}

fn lexically_compare(constraint: &Constraint, other: impl ToString) -> bool {
    let target = constraint.revision().to_string();
    let other = other.to_string();
    for ord in ords_for(constraint) {
        if lexical_sort::lexical_cmp(&target, &other) == ord {
            return true;
        }
    }
    return false;
}

#[cfg(test)]
mod tests {
    use simple_test_case::test_case;

    use super::*;
    use crate::{Revision, constraint};

    // Using a fetcher that is unlikely to ever have actual comparison functionality.
    const FETCHER: Fetcher = Fetcher::Archive;

    #[test_case(constraint!(Compatible => 1, 2, 3), Revision::from("1.2.3"), true; "1.2.3_compatible_1.2.3")]
    #[test_case(constraint!(Compatible => 1, 2, 3), Revision::from("1.2.4"), true; "1.2.4_compatible_1.2.3")]
    #[test_case(constraint!(Compatible => 1, 2, 3), Revision::from("2.0.0"), false; "2.0.0_not_compatible_1.2.3")]
    #[test_case(constraint!(Equal => 1, 2, 3), Revision::from("1.2.3"), true; "1.2.3_equal_1.2.3")]
    #[test_case(constraint!(Equal => 1, 2, 3), Revision::from("1.2.4"), false; "1.2.4_not_equal_1.2.3")]
    #[test_case(constraint!(NotEqual => 1, 2, 3), Revision::from("1.2.3"), false; "1.2.3_not_notequal_1.2.3")]
    #[test_case(constraint!(NotEqual => 1, 2, 3), Revision::from("1.2.4"), true; "1.2.4_notequal_1.2.3")]
    #[test_case(constraint!(Less => 1, 2, 3), Revision::from("1.2.2"), true; "1.2.2_less_1.2.3")]
    #[test_case(constraint!(Less => 1, 2, 3), Revision::from("1.2.3"), false; "1.2.3_not_less_1.2.3")]
    #[test_case(constraint!(LessOrEqual => 1, 2, 3), Revision::from("1.2.2"), true; "1.2.2_less_or_equal_1.2.3")]
    #[test_case(constraint!(LessOrEqual => 1, 2, 3), Revision::from("1.2.3"), true; "1.2.3_less_or_equal_1.2.3")]
    #[test_case(constraint!(LessOrEqual => 1, 2, 3), Revision::from("1.2.4"), false; "1.2.4_not_less_or_equal_1.2.3")]
    #[test_case(constraint!(Greater => 1, 2, 3), Revision::from("1.2.4"), true; "1.2.4_greater_1.2.3")]
    #[test_case(constraint!(Greater => 1, 2, 3), Revision::from("1.2.3"), false; "1.2.3_not_greater_1.2.3")]
    #[test_case(constraint!(GreaterOrEqual => 1, 2, 3), Revision::from("1.2.4"), true; "1.2.4_greater_or_equal_1.2.3")]
    #[test_case(constraint!(GreaterOrEqual => 1, 2, 3), Revision::from("1.2.3"), true; "1.2.3_greater_or_equal_1.2.3")]
    #[test_case(constraint!(GreaterOrEqual => 1, 2, 3), Revision::from("1.2.2"), false; "1.2.2_not_greater_or_equal_1.2.3")]
    #[test]
    fn compare_semver(constraint: Constraint, target: Revision, expected: bool) {
        assert_eq!(
            compare(&constraint, FETCHER, &target),
            expected,
            "compare '{target}' to '{constraint}', expected: {expected}"
        );
    }

    #[test_case(constraint!(Compatible => "abcd"), Revision::from("AbCd"), true; "abcd_compatible_AbCd")]
    #[test_case(constraint!(Compatible => "abcd"), Revision::from("AbCdE"), false; "abcd_not_compatible_AbCdE")]
    #[test_case(constraint!(Equal => "abcd"), Revision::from("abcd"), true; "abcd_equal_abcd")]
    #[test_case(constraint!(Equal => "abcd"), Revision::from("aBcD"), false; "abcd_not_equal_aBcD")]
    #[test_case(constraint!(NotEqual => "abcd"), Revision::from("abcde"), true; "abcd_notequal_abcde")]
    #[test_case(constraint!(NotEqual => "abcd"), Revision::from("abcd"), false; "abcd_not_notequal_abcd")]
    #[test_case(constraint!(Less => "a"), Revision::from("b"), true; "a_less_b")]
    #[test_case(constraint!(Less => "a"), Revision::from("a"), false; "a_not_less_a")]
    #[test_case(constraint!(Greater => "b"), Revision::from("a"), true; "b_greater_a")]
    #[test_case(constraint!(Greater => "b"), Revision::from("c"), false; "b_not_greater_c")]
    #[test_case(constraint!(Less => "あ"), Revision::from("え"), true; "jp_a_less_e")]
    #[test_case(constraint!(Greater => "え"), Revision::from("あ"), true; "jp_e_greater_a")]
    #[test_case(constraint!(Equal => "あ"), Revision::from("あ"), true; "jp_a_equal_a")]
    #[test_case(constraint!(Compatible => "Maße"), Revision::from("MASSE"), true; "gr_masse_compatible_MASSE")]
    #[test]
    fn compare_opaque(constraint: Constraint, target: Revision, expected: bool) {
        assert_eq!(
            compare(&constraint, FETCHER, &target),
            expected,
            "compare '{target}' to '{constraint}', expected: {expected}"
        );
    }
}
