use semver::Version as SemVer;
use simple_test_case::test_case;

use locator::{Constraint, Revision, constraint, revision};

#[test_case(constraint!(Compatible => revision!(1, 2, 3)), revision!(parse => "1.2.3"), true; "1.2.3_compatible_1.2.3")]
#[test_case(constraint!(Compatible => revision!(1, 2, 3)), revision!(parse => "1.2.4"), true; "1.2.4_compatible_1.2.3")]
#[test_case(constraint!(Compatible => revision!(1, 2, 3)), revision!(parse => "2.0.0"), false; "2.0.0_not_compatible_1.2.3")]
#[test_case(constraint!(Equal => revision!(1, 2, 3)), revision!(parse => "1.2.3"), true; "1.2.3_equal_1.2.3")]
#[test_case(constraint!(Equal => revision!(1, 2, 3)), revision!(parse => "1.2.4"), false; "1.2.4_not_equal_1.2.3")]
#[test_case(constraint!(NotEqual => revision!(1, 2, 3)), revision!(parse => "1.2.3"), false; "1.2.3_not_notequal_1.2.3")]
#[test_case(constraint!(NotEqual => revision!(1, 2, 3)), revision!(parse => "1.2.4"), true; "1.2.4_notequal_1.2.3")]
#[test_case(constraint!(Less => revision!(1, 2, 3)), revision!(parse => "1.2.2"), true; "1.2.2_less_1.2.3")]
#[test_case(constraint!(Less => revision!(1, 2, 3)), revision!(parse => "1.2.3"), false; "1.2.3_not_less_1.2.3")]
#[test_case(constraint!(LessOrEqual => revision!(1, 2, 3)), revision!(parse => "1.2.2"), true; "1.2.2_less_or_equal_1.2.3")]
#[test_case(constraint!(LessOrEqual => revision!(1, 2, 3)), revision!(parse => "1.2.3"), true; "1.2.3_less_or_equal_1.2.3")]
#[test_case(constraint!(LessOrEqual => revision!(1, 2, 3)), revision!(parse => "1.2.4"), false; "1.2.4_not_less_or_equal_1.2.3")]
#[test_case(constraint!(Greater => revision!(1, 2, 3)), revision!(parse => "1.2.4"), true; "1.2.4_greater_1.2.3")]
#[test_case(constraint!(Greater => revision!(1, 2, 3)), revision!(parse => "1.2.3"), false; "1.2.3_not_greater_1.2.3")]
#[test_case(constraint!(GreaterOrEqual => revision!(1, 2, 3)), revision!(parse => "1.2.4"), true; "1.2.4_greater_or_equal_1.2.3")]
#[test_case(constraint!(GreaterOrEqual => revision!(1, 2, 3)), revision!(parse => "1.2.3"), true; "1.2.3_greater_or_equal_1.2.3")]
#[test_case(constraint!(GreaterOrEqual => revision!(1, 2, 3)), revision!(parse => "1.2.2"), false; "1.2.2_not_greater_or_equal_1.2.3")]
#[test]
fn compare_revisions_semver(constraint: Constraint<Revision>, target: Revision, expected: bool) {
    assert_eq!(
        constraint.matches(&target),
        expected,
        "check if version '{target:?}' matches constraint '{constraint:?}, expected: {expected}"
    );
}

#[test_case(constraint!(Compatible => revision!("abcd")), revision!(parse => "AbCd"), true; "abcd_compatible_AbCd")]
#[test_case(constraint!(Compatible => revision!("abcd")), revision!(parse => "AbCdE"), false; "abcd_not_compatible_AbCdE")]
#[test_case(constraint!(Equal => revision!("abcd")), revision!(parse => "abcd"), true; "abcd_equal_abcd")]
#[test_case(constraint!(Equal => revision!("abcd")), revision!(parse => "aBcD"), false; "abcd_not_equal_aBcD")]
#[test_case(constraint!(NotEqual => revision!("abcd")), revision!(parse => "abcde"), true; "abcd_notequal_abcde")]
#[test_case(constraint!(NotEqual => revision!("abcd")), revision!(parse => "abcd"), false; "abcd_not_notequal_abcd")]
#[test_case(constraint!(Less => revision!("a")), revision!(parse => "b"), true; "a_less_b")]
#[test_case(constraint!(Less => revision!("a")), revision!(parse => "a"), false; "a_not_less_a")]
#[test_case(constraint!(Greater => revision!("b")), revision!(parse => "a"), true; "b_greater_a")]
#[test_case(constraint!(Greater => revision!("b")), revision!(parse => "c"), false; "b_not_greater_c")]
#[test_case(constraint!(Less => revision!("あ")), revision!(parse => "え"), true; "jp_a_less_e")]
#[test_case(constraint!(Greater => revision!("え")), revision!(parse => "あ"), true; "jp_e_greater_a")]
#[test_case(constraint!(Equal => revision!("あ")), revision!(parse => "あ"), true; "jp_a_equal_a")]
#[test_case(constraint!(Compatible => revision!("Maße")), revision!(parse => "MASSE"), true; "gr_masse_compatible_MASSE")]
#[test]
fn compare_revisions_opaque(constraint: Constraint<Revision>, target: Revision, expected: bool) {
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
