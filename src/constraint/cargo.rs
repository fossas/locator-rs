use super::Constraint;
use crate::Revision;
use semver::VersionReq;
use thiserror::Error;

/// Conveniently, the `semver` crate implements Cargo's flavor of SemVer.
/// See: https://docs.rs/semver/latest/semver/index.html
///
/// This means we can rely on [`VersionReq::matches`] to determine if the
/// revision satisfies the constraint.
pub fn compare(constraint: &Constraint, revision: &Revision) -> Result<bool, CargoContraintError> {
    let req = match VersionReq::parse(&constraint.to_string()) {
        Ok(req) => req,
        Err(error) => {
            return Err(CargoContraintError::InvalidConstraint(
                constraint.clone(),
                error,
            ));
        }
    };

    let Revision::Semver(version) = revision else {
        tracing::warn!(%revision, "Cargo revision is not SemVer");
        return Err(CargoContraintError::NotSemVer(revision.clone()));
    };

    Ok(req.matches(version))
}

#[derive(Error, Debug)]
pub enum CargoContraintError {
    #[error("invalid cargo version requirement")]
    InvalidConstraint(Constraint, semver::Error),
    #[error("carog revision is not semver")]
    NotSemVer(Revision),
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
            compare(&constraint, &target).expect("should compare"),
            expected,
            "compare '{target}' to '{constraint}', expected: {expected}"
        );
    }
}
