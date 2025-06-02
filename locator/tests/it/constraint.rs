//! Tests for constraint parsing.

use simple_test_case::test_case;

use locator::{
    Locator, Revision, StrictLocator, constraint, constraint::*, constraints, locator, revision,
    strict,
};

mod fallback;
mod nuget;

#[test_case(constraint!(Compatible => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.4"); "1.2.4_compatible_1.2.3")]
#[test_case(constraint!(Equal => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.3"); "1.2.3_equal_1.2.3")]
#[test_case(constraint!(NotEqual => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.4"); "1.2.4_notequal_1.2.3")]
#[test_case(constraint!(Less => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.2"); "1.2.2_less_1.2.3")]
#[test_case(constraint!(LessOrEqual => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.2"); "1.2.2_less_or_equal_1.2.3")]
#[test_case(constraint!(Greater => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.4"); "1.2.4_greater_1.2.3")]
#[test_case(constraint!(GreaterOrEqual => revision!(1, 2, 3)), locator!(Archive, "pkg", "1.2.4"); "1.2.4_greater_or_equal_1.2.3")]
#[test]
fn constraint_locator(constraint: Constraint<Revision>, target: Locator) {
    let revision = target.revision().expect("must have a revision");
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
    let revision = target.revision().expect("must have a revision");
    assert!(
        constraints.any_match(revision),
        "version '{target}' should match at least one constraint in '{constraints:?}'"
    );
}

#[test_case(constraints!({ Greater => revision!(1, 2, 3) }, { Less => revision!(2, 0, 0) }), locator!(Archive, "pkg", "1.2.4"); "1.2.4_greater_1.2.3_and_less_2.0.0")]
#[test_case(constraints!({ Less => revision!("abcd") }, { Greater => revision!("bbbb") }), locator!(Archive, "pkg", "abce"); "abce_greater_abcd_and_less_bbbb")]
#[test]
fn constraints_locator_all(constraints: Constraints<Revision>, target: Locator) {
    let revision = target.revision().expect("must have a revision");
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

#[test_case("=1.2.3", "1.2.3", true; "eq1.2.3_includes_1.2.3")]
#[test_case("=1.2.3", "1.2.4", false; "eq1.2.3_excludes_1.2.4")]
#[test_case("=1.2", "1.2.3", true; "eq1.2_includes_1.2.3")]
#[test_case("=1.2", "1.3.0", false; "eq1.2_excludes_1.3.0")]
#[test_case("=1", "1.2.3", true; "eq1_includes_1.2.3")]
#[test_case("=1", "2.0.0", false; "eq1_excludes_2.0.0")]
#[test_case(">1.2.3", "1.2.4", true; "gt1.2.3_includes_1.2.4")]
#[test_case(">1.2.3", "1.2.2", false; "gt1.2.3_excludes_1.2.2")]
#[test_case(">1.2", "1.3.0", true; "gt1.2_includes_1.3.0")]
#[test_case(">1.2", "1.2.3", false; "gt1.2_excludes_1.2.3")]
#[test_case(">1", "2.0.0", true; "gt1_includes_2.0.0")]
#[test_case(">1", "1.2.3", false; "gt1_excludes_1.2.3")]
#[test_case(">=1.2.3", "1.2.4", true; "gte1.2.3_includes_1.2.4")]
#[test_case(">=1.2.3", "1.2.2", false; "gte1.2.3_excludes_1.2.2")]
#[test_case(">=1.2", "1.3.0", true; "gte1.2_includes_1.3.0")]
#[test_case(">=1.2", "1.1.3", false; "gte1.2_excludes_1.1.3")]
#[test_case(">=1", "2.0.0", true; "gte1_includes_2.0.0")]
#[test_case(">=1", "0.9.9", false; "gte1_excludes_0.9.9")]
#[test_case("<=1.2.3", "1.2.3", true; "lte1.2.3_includes_1.2.3")]
#[test_case("<=1.2.3", "1.2.4", false; "lte1.2.3_excludes_1.2.4")]
#[test_case("<=1.2", "1.2.9", true; "lte1.2_includes_1.2.9")]
#[test_case("<=1.2", "1.3.0", false; "lte1.2_excludes_1.3.0")]
#[test_case("<=1", "1.9.9", true; "lte1_includes_1.9.9")]
#[test_case("<=1", "2.0.0", false; "lte1_excludes_2.0.0")]
#[test_case("<1.2.3", "1.2.2", true; "lt1.2.3_includes_1.2.2")]
#[test_case("<1.2.3", "1.2.4", false; "lt1.2.3_excludes_1.2.4")]
#[test_case("<1.2", "1.1.9", true; "lt1.2_includes_1.1.9")]
#[test_case("<1.2", "1.2.0", false; "lt1.2_excludes_1.2.0")]
#[test_case("<1", "0.9.9", true; "lt1_includes_0.9.9")]
#[test_case("<1", "1.2.3", false; "lt1_excludes_1.2.3")]
#[test_case("~1.2.3", "1.2.4", true; "tilde1.2.3_includes_1.2.4")]
#[test_case("~1.2.3", "1.3.0", false; "tilde1.2.3_excludes_1.3.0")]
#[test_case("~1.2", "1.2.0", true; "tilde1.2_includes_1.2.0")]
#[test_case("~1.2", "1.3.0", false; "tilde1.2_excludes_1.3.0")]
#[test_case("~1", "1.0.0", true; "tilde1_includes_1.0.0")]
#[test_case("~1", "2.0.0", false; "tilde1_excludes_2.0.0")]
#[test_case("^1.2.3", "1.9.9", true; "caret1.2.3_includes_1.9.9")]
#[test_case("^1.2.3", "2.0.0", false; "caret1.2.3_excludes_2.0.0")]
#[test_case("^0.2.3", "0.2.9", true; "caret0.2.3_includes_0.2.9")]
#[test_case("^0.2.3", "0.3.0", false; "caret0.2.3_excludes_0.3.0")]
#[test_case("^0.0.3", "0.0.3", true; "caret0.0.3_includes_0.0.3")]
#[test_case("^0.0.3", "0.0.4", false; "caret0.0.3_excludes_0.0.4")]
#[test_case("^1.2", "1.9.9", true; "caret1.2_includes_1.9.9")]
#[test_case("^1.2", "2.0.0", false; "caret1.2_excludes_2.0.0")]
#[test_case("^0.2", "0.2.9", true; "caret0.2_includes_0.2.9")]
#[test_case("^0.2", "0.3.0", false; "caret0.2_excludes_0.3.0")]
#[test_case("^0.0", "0.0.0", true; "caret0.0_includes_0.0.0")]
#[test_case("^0.0", "0.1.0", false; "caret0.0_excludes_0.1.0")]
#[test_case("^1", "1.9.9", true; "caret1_includes_1.9.9")]
#[test_case("^1", "2.0.0", false; "caret1_excludes_2.0.0")]
#[test_case("1.2.*", "1.2.3", true; "1.2.wc_includes_1.2.3")]
#[test_case("1.2.*", "1.3.0", false; "1.2.wc_excludes_1.3.0")]
#[test_case("1.*", "1.2.3", true; "1.wc_includes_1.2.3")]
#[test_case("1.*", "2.0.0", false; "1.wc_excludes_2.0.0")]
#[test_case("1.*.*", "1.2.3", true; "1.wc.wc_includes_1.2.3")]
#[test_case("1.*.*", "2.0.0", false; "1.wc.wc_excludes_2.0.0")]
#[test]
fn cargo_parse_and_compare(req: &str, ver: &str, expected: bool) {
    let req = locator::constraint::cargo::parse(req).expect("parse constraint");
    let ver = Revision::try_from(ver).expect("parse revision");
    assert_eq!(
        req.all_match(&ver),
        expected,
        "Expected {req:?} to match {ver:?}"
    );
}

#[test_case("=1.2.3", "1.2.3", true; "eq1.2.3_includes_1.2.3")]
#[test_case("=1.2.3", "1.2.4", false; "eq1.2.3_excludes_1.2.4")]
#[test_case(">1.2.3", "1.2.4", true; "gt1.2.3_includes_1.2.4")]
#[test_case(">1.2.3", "1.2.2", false; "gt1.2.3_excludes_1.2.2")]
#[test_case("<1.2.3", "1.2.2", true; "lt1.2.3_includes_1.2.2")]
#[test_case("<1.2.3", "1.2.4", false; "lt1.2.3_excludes_1.2.4")]
#[test_case(">=1.2.3", "1.2.3", true; "gte1.2.3_includes_1.2.3")]
#[test_case(">=1.2.3", "1.2.2", false; "gte1.2.3_excludes_1.2.2")]
#[test_case("<=1.2.3", "1.2.3", true; "lte1.2.3_includes_1.2.3")]
#[test_case("<=1.2.3", "1.2.4", false; "lte1.2.3_excludes_1.2.4")]
#[test_case("~1.2.3", "1.2.4", true; "tilde1.2.3_includes_1.2.4")]
#[test_case("~1.2.3", "1.3.0", true; "tilde1.2.3_includes_1.3.0")]
#[test_case("^1.2.3", "1.9.9", true; "caret1.2.3_includes_1.9.9")]
#[test_case("^1.2.3", "2.0.0", false; "caret1.2.3_excludes_2.0.0")]
#[test_case("!=1.2.3", "1.2.4", true; "neq1.2.3_includes_1.2.4")]
#[test_case("!=1.2.3", "1.2.3", false; "neq1.2.3_excludes_1.2.3")]
#[test_case(">1.0.0,<2.0.0", "1.5.0", true; "range_includes_1.5.0")]
#[test_case(">1.0.0,<2.0.0", "2.0.0", false; "range_excludes_2.0.0")]
#[test_case(">=1.0.0,<=1.5.0", "1.5.0", true; "inclusive_range_includes_1.5.0")]
#[test_case(">=1.0.0,<=1.5.0", "1.5.1", false; "inclusive_range_excludes_1.5.1")]
#[test_case("^1.0.0,!=1.2.3", "1.2.3", false; "complex_condition_excludes_1.2.3")]
#[test_case("^1.0.0,!=1.2.3", "1.3.0", true; "complex_condition_includes_1.3.0")]
#[test]
fn arbitrary_parse_and_compare(req: &str, ver: &str, expected: bool) {
    let req = locator::constraint::parse(req).expect("parse constraint");
    let ver = Revision::try_from(ver).expect("parse revision");
    assert_eq!(
        req.all_match(&ver),
        expected,
        "Expected {req:?} to match {ver:?}"
    );
}
