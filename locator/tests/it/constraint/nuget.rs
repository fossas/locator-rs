use std::cmp::Ordering;

use simple_test_case::test_case;

use locator::{Constraint, Constraints, constraint, constraint::nuget::*, constraints};

// Define a macro to create NuGet version constraints more easily
macro_rules! version {
    ($major:expr, $minor:expr, $patch:expr) => {
        Requirement::try_from(format!("{}.{}.{}", $major, $minor, $patch)).unwrap()
    };
    ($major:expr, $minor:expr, $patch:expr, $revision:expr) => {
        Requirement::try_from(format!("{}.{}.{}.{}", $major, $minor, $patch, $revision)).unwrap()
    };
    ($version:expr) => {
        Requirement::try_from($version.to_string()).unwrap()
    };
}

#[test_case("= 1.0.0", constraints!({ Equal => version!(1, 0, 0) }); "1.0.0_eq_1.0.0")]
#[test_case(
    ">= 1.0, < 2.0",
    constraints!(
        { GreaterOrEqual => version!("1.0") },
        { Less => version!("2.0") }
    );
    "1.0_geq_1.0_AND_lt_2.0"
)]
#[test_case("> 1.0.0-alpha", constraints!({ Greater => version!("1.0.0-alpha") }); "1.0.0-alpha_gt_1.0.0-alpha")]
#[test_case("<= 2.0.0", constraints!({ LessOrEqual => version!("2.0.0") }); "2.0.0_leq_2.0.0")]
#[test_case("= 1.0.0.1", constraints!({ Equal => version!(1, 0, 0, 1) }); "1.0.0.1_eq_1.0.0.1")]
#[test]
fn nuget_constraints_parsing(input: &str, expected: Constraints<Requirement>) {
    // Parse the constraints string to Constraint<Revision>
    let parsed_revisions = parse_constraints(input).expect("should parse constraint");

    // Convert from Constraint<Revision> to Constraint<Requirement>
    let parsed_versions = parsed_revisions
        .into_iter()
        .map(|constraint| constraint.map(|rev| Requirement::try_from(rev.to_string()).unwrap()))
        .collect::<Vec<Constraint<Requirement>>>();

    // Create Constraints from the vector
    let parsed = Constraints::from(parsed_versions);

    // Compare expected vs actual
    assert_eq!(
        format!("{:?}", expected),
        format!("{:?}", parsed),
        "compare constraints: expected={:?}, parsed={:?}",
        expected,
        parsed
    );
}

#[test_case("!= 1.9.3"; "unsupported_operator_neq")]
#[test_case("~= 2.5"; "unsupported_operator_compat")]
#[test_case("$%!@#"; "invalid_special_chars")]
#[test_case("1.2.3 !!"; "trailing_invalid_chars")]
#[test_case(">>= 1.0"; "invalid_operator")]
#[test_case("~= "; "missing_version_after_operator")]
#[test_case(">= 1.0,"; "trailing_comma")]
#[test]
fn nuget_constraints_parsing_failure(input: &str) {
    parse_constraints(input).expect_err("should not parse constraint");
}

#[test_case(constraint!(Equal => version!(1, 0, 0)), version!(1, 0, 0), true; "equal_versions")]
#[test_case(constraint!(Equal => version!("1.0")), version!(1, 0, 0), true; "equal_normalized_versions")]
#[test_case(constraint!(Equal => version!("1")), version!(1, 0, 0), true; "equal_normalized_versions_major")]
#[test_case(constraint!(Equal => version!("1.0.0.0")), version!("1.0"), true; "equal_normalized_versions_with_zeros")]
#[test_case(constraint!(Equal => version!(1, 0, 0)), version!(1, 0, 0, 1), false; "unequal_with_revision")]
#[test_case(constraint!(Equal => version!("1.0.0-alpha")), version!("1.0.0-ALPHA"), true; "equal_case_insensitive_prerelease")]
#[test_case(constraint!(GreaterOrEqual => version!(1, 0, 0)), version!(1, 0, 0), true; "greater_equal_same")]
#[test_case(constraint!(GreaterOrEqual => version!(1, 0, 0)), version!(0, 9, 0), false; "not_greater_equal")]
#[test_case(constraint!(Less => version!(2, 0, 0)), version!(1, 9, 9), true; "less_than")]
#[test_case(constraint!(Less => version!(2, 0, 0)), version!(1, 9, 9, 9), true; "less_than_with_revision")]
#[test_case(constraint!(Less => version!(2, 0, 0, 1)), version!(1, 9, 9, 9), true; "less_than_with_revisions")]
#[test_case(constraint!(Less => version!("1.0")), version!("1"), false; "not_less_equal")]
#[test_case(constraint!(Greater => version!("1.0.0-alpha")), version!(1, 0, 0), true; "release_greater_than_prerelease")]
#[test_case(constraint!(Greater => version!("1.0.0-alpha")), version!("1.0.0-beta"), true; "beta_greater_than_alpha")]
#[test_case(constraint!(Equal => version!("1.0.0+metadata")), version!(1, 0, 0), true; "ignore_metadata_in_comparison")]
#[test_case(constraint!(Compatible => version!(1, 2, 3)), version!(1, 2, 4), true; "compatible_within_minor")]
#[test_case(constraint!(Compatible => version!(1, 2, 3)), version!(1, 3, 0), false; "not_compatible_higher_minor")]
#[test_case(constraint!(Compatible => version!(1, 2, 0)), version!(1, 9, 9), true; "compatible_within_major")]
#[test_case(constraint!(Compatible => version!("1.0.0.0")), version!(1, 9, 9), true; "compatible_within_major_zeros")]
#[test_case(constraint!(Compatible => version!(1, 2, 3, 4)), version!("1.2.3.9"), true; "compatible_within_patch")]
#[test]
fn nuget_version_comparison(
    constraint: Constraint<Requirement>,
    target: Requirement,
    expected: bool,
) {
    assert_eq!(
        constraint.matches(&target),
        expected,
        "Constraint '{constraint}' match with '{target}' should be {expected}"
    );
}

#[test_case("1.2.3", "1.2.3", Ordering::Equal; "equal_normal_versions")]
#[test_case("2.1.0", "1.9.0", Ordering::Greater; "greater_minor_version")]
#[test_case("1.2.0", "1.8.0", Ordering::Less; "less_minor_version")]
#[test_case("2.1.0", "2.1.0-alpha", Ordering::Greater; "release_greater_than_prerelease")]
#[test_case("3.2.1-alpha", "3.2.1-beta", Ordering::Less; "alpha_less_than_beta")]
#[test_case("4.5", "4.5.0", Ordering::Equal; "normalized_version_comparison")]
#[test_case("5.6.7.0", "5.6.7", Ordering::Equal; "trailing_zeros_ignored")]
#[test_case("6.7.8.1", "6.7.8", Ordering::Greater; "revision_matters")]
#[test_case("7.8.9-alpha", "7.8.9-ALPHA", Ordering::Equal; "case_insensitive_prerelease_v1")]
#[test_case("7.8.9.10-alpha", "7.8.9.10-ALPHA", Ordering::Equal; "case_insensitive_prerelease_v2")]
#[test]
fn nuget_version_ordering(lhs: &str, rhs: &str, expected: Ordering) {
    let v1 = Requirement::try_from(lhs.to_string()).expect("valid version");
    let v2 = Requirement::try_from(rhs.to_string()).expect("valid version");
    assert_eq!(
        v1.cmp(&v2),
        expected,
        "Expected {lhs} to be {expected:?} {rhs}"
    );
}
