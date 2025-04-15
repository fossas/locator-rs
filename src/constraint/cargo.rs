use semver::VersionReq;

use crate::{Fetcher, Revision, constraint::fallback};

use super::Constraint;

/// Conveniently, the `semver` crate implements Cargo's flavor of SemVer.
/// See: https://docs.rs/semver/latest/semver/index.html
///
/// This means we can rely on [`VersionReq::matches`] to determine if the
/// revision satisfies the constraint.
pub fn compare(constraint: &Constraint, revision: &Revision) -> bool {
    let req = match VersionReq::parse(&constraint.to_string()) {
        Ok(req) => req,
        Err(error) => {
            tracing::warn!(
                %constraint,
                ?error,
                "Invalid Cargo version requirement"
            );
            return fallback::compare(constraint, Fetcher::Cargo, revision);
        }
    };

    let Revision::Semver(version) = revision else {
        tracing::warn!(%revision, "Cargo revision is not SemVer");
        return fallback::compare(constraint, Fetcher::Cargo, revision);
    };

    req.matches(version)
}

#[cfg(test)]
mod tests {
    use simple_test_case::test_case;

    use super::*;
    use crate::{Revision, constraint};

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
            compare(&constraint, &target),
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
            compare(&constraint, &target),
            expected,
            "compare '{target}' to '{constraint}', expected: {expected}"
        );
    }
}
