//! # NuGet Versioning.
//!
//! We get NuGet vulnerabilities from [GHSA](https://github.com/advisories?query=type%3Areviewed+ecosystem%3Anuget)
//!
//! ## NuGet Versioning and SemVer 2
//!
//! NuGet versions are generally compatible with SemVer 2.
//! They document their differences from [here](https://learn.microsoft.com/en-us/nuget/concepts/package-versioning?tabs=semver20sort#where-nugetversion-diverges-from-semantic-versioning):
//!
//! Briefly:
//! - NuGetVersion supports a 4th version segment, Revision. So they are `Major.Minor.Patch.Revision`.
//! - NuGetVersion only requires the major segment to be defined. All others are optional, and are equivalent to zero. This means that 1, 1.0, 1.0.0, and 1.0.0.0 are all accepted and equal.
//! - NuGetVersion uses case insensitive string comparisons for pre-release components. This means that 1.0.0-alpha and 1.0.0-Alpha are equal.
//!
//! ## Version normalization:
//!
//! See [here for original documentation](https://learn.microsoft.com/en-us/nuget/concepts/package-versioning?tabs=semver20sort#normalized-version-numbers)
//!
//! - Leading zeroes are removed  from version numbers:
//!     * 1.00 => 1.0
//!     * 1.01.1 => 1.1.1
//!     * 1.00.0.1 => 1.0.0.1
//!
//! - A revision of 0 is omitted.
//!     * 1.0.0.0 => 1.0.0
//!     * 1.0.0.1 => 1.0.0.1
//!
//! - Semver metadata is removed.
//!     *  1.0.7+r3456 => 1.0.7
//!

use std::cmp::Ordering;

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, multispace0, u64},
    combinator::{eof, map_res, not, opt, recognize},
    multi::separated_list1,
    sequence::{delimited, pair, preceded, terminated},
};
use thiserror::Error;

use super::Constraint;
use crate::{Fetcher, Revision};

#[tracing::instrument]
pub fn compare(
    constraint: &Constraint,
    fetcher: Fetcher,
    target: &Revision,
) -> Result<bool, NuGetConstraintError> {
    let threshold = NuGetVersion::try_from(constraint.revision())?;
    let target = NuGetVersion::try_from(target)?;

    // Special case for Equal with prerelease identifiers that might differ in case.
    // For details see [here](https://learn.microsoft.com/en-us/nuget/concepts/package-versioning?tabs=semver20sort#where-nugetversion-diverges-from-semantic-versioning).
    if let Constraint::Equal(_) = constraint {
        if let (Some(threshold_pre), Some(target_pre)) = (&threshold.prerelease, &target.prerelease)
        {
            if threshold_pre.to_lowercase() == target_pre.to_lowercase()
                && threshold.major == target.major
                && threshold.minor == target.minor
                && threshold.patch == target.patch
                && threshold.revision == target.revision
            {
                return Ok(true);
            }
        }
    }

    Ok(match constraint {
        Constraint::Equal(_) => target == threshold,
        Constraint::NotEqual(_) => target != threshold,
        Constraint::Less(_) => target < threshold,
        Constraint::LessOrEqual(_) => target <= threshold,
        Constraint::Greater(_) => target > threshold,
        Constraint::GreaterOrEqual(_) => target >= threshold,
        Constraint::Compatible(_) => {
            let min_version = threshold;
            let mut max_version = min_version.clone();
            if min_version.minor > 0 || min_version.patch > 0 || min_version.revision > 0 {
                if min_version.revision > 0 {
                    max_version.patch += 1;
                    max_version.revision = 0;
                } else if min_version.patch > 0 {
                    max_version.minor += 1;
                    max_version.patch = 0;
                    max_version.revision = 0;
                } else {
                    max_version.major += 1;
                    max_version.minor = 0;
                    max_version.patch = 0;
                    max_version.revision = 0;
                }
            }
            target >= min_version && target < max_version
        }
    })
}

/// Parses a string into a vector of NuGet constraints.
///
/// # Origin
///
/// - These constraints come from [the GHSA global advisories API](https://docs.github.com/en/rest/security-advisories/global-advisories?apiVersion=2022-11-28).
/// - They are found in, in jql, `.[].vulnerabilities[].vulnerable_version_range`
/// - They correspond to the underlying [OSV ranges](https://ossf.github.io/osv-schema/#affectedranges-field)
///
/// # Format
///
/// These constraints are represented by either:
/// - A single constraint with an operator (`>=`, `<=`, `>`, `<`, `=`) and a version.
/// - Multiple comma-separated constraints.
///
#[tracing::instrument]
pub fn parse_constraints(input: &str) -> Result<Vec<Constraint>, NuGetConstraintError> {
    fn operator(input: &str) -> IResult<&str, &str> {
        alt((tag("="), tag(">="), tag("<="), tag(">"), tag("<"))).parse(input)
    }

    fn version(input: &str) -> IResult<&str, &str> {
        take_while1(|c: char| c.is_alphanumeric() || c == '.' || c == '-' || c == '+').parse(input)
    }

    fn single_constraint(input: &str) -> IResult<&str, Constraint> {
        let (input, (op, ver)) = pair(
            delimited(multispace0, operator, multispace0),
            delimited(multispace0, version, multispace0),
        )
        .parse(input)?;

        let rev = Revision::Opaque(ver.to_string());

        let constraint = match op {
            "=" => Constraint::Equal(rev),
            ">" => Constraint::Greater(rev),
            ">=" => Constraint::GreaterOrEqual(rev),
            "<" => Constraint::Less(rev),
            "<=" => Constraint::LessOrEqual(rev),
            _ => {
                return Err(nom::Err::Failure(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Tag,
                )));
            }
        };

        Ok((input, constraint))
    }

    fn constraints(input: &str) -> IResult<&str, Vec<Constraint>> {
        terminated(
            separated_list1(
                delimited(multispace0, char(','), multispace0),
                single_constraint,
            ),
            eof,
        )
        .parse(input)
    }

    constraints(input.trim())
        .map(|(_, parsed)| parsed)
        .map_err(|e| NuGetConstraintError::ConstraintParse {
            constraints: input.to_string(),
            message: format!("failed to parse constraint: {e:?}"),
        })
}

/// Errors from NuGet constraints
#[derive(Error, Clone, PartialEq, Eq, Debug)]
pub enum NuGetConstraintError {
    #[error("parse version {version:?}: {message:?})")]
    VersionParse { version: String, message: String },

    #[error("parse constraints {constraints:?}: {message:?})")]
    ConstraintParse {
        constraints: String,
        message: String,
    },
}

/// A NuGet version.
/// Major.Minor.Patch.Revision format with optional prerelease labels.
#[derive(Debug, Clone, PartialEq, Eq)]
struct NuGetVersion {
    /// The major version as in semver.
    major: u64,

    /// The minor version as in semver.
    minor: u64,

    /// The patch version as in semver.
    patch: u64,

    /// The revision version as *absent from* semver.
    revision: u64,

    /// Optional prerelease labels (case-insensitive in NuGet)
    prerelease: Option<String>,
}

impl std::fmt::Display for NuGetVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let major = self.major;
        let minor = self.minor;
        let patch = self.patch;
        let revision = self.revision;
        if let Some(prerelease) = &self.prerelease {
            write!(
                f,
                "{}.{}.{}.{}-{}",
                major, minor, patch, revision, prerelease
            )
        } else {
            write!(f, "{}.{}.{}.{}", major, minor, patch, revision)
        }
    }
}

impl NuGetVersion {
    fn parse(input: &str) -> IResult<&str, Self> {
        fn segments(input: &str) -> IResult<&str, (u64, u64, u64, u64)> {
            let (input, major) = u64.parse(input)?;

            let (input, minor) = opt(preceded(char('.'), u64))
                .map(|m| m.unwrap_or(0))
                .parse(input)?;

            let (input, patch) = opt(preceded(char('.'), u64))
                .map(|p| p.unwrap_or(0))
                .parse(input)?;

            let (input, revision) = opt(preceded(char('.'), u64))
                .map(|r| r.unwrap_or(0))
                .parse(input)?;

            Ok((input, (major, minor, patch, revision)))
        }

        fn prerelease(input: &str) -> IResult<&str, String> {
            preceded(
                char('-'),
                map_res(
                    take_while1(|c: char| c.is_alphanumeric() || c == '.' || c == '-'),
                    |s: &str| -> Result<String, std::convert::Infallible> { Ok(s.to_string()) },
                ),
            )
            .parse(input)
        }

        fn metadata_part(input: &str) -> IResult<&str, ()> {
            preceded(
                char('+'),
                preceded(
                    not(eof),
                    map_res(
                        recognize(take_while1(|c: char| {
                            c.is_alphanumeric() || c == '.' || c == '-'
                        })),
                        |_| -> Result<(), std::convert::Infallible> { Ok(()) },
                    ),
                ),
            )
            .parse(input)
        }

        let (input, ((major, minor, patch, revision), pre, _)) =
            (segments, opt(prerelease), opt(metadata_part)).parse(input)?;

        Ok((
            input,
            NuGetVersion {
                major,
                minor,
                patch,
                revision,
                prerelease: pre,
            },
        ))
    }
}

impl TryFrom<&Revision> for NuGetVersion {
    type Error = NuGetConstraintError;

    fn try_from(rev: &Revision) -> Result<Self, Self::Error> {
        match rev {
            Revision::Semver(semver) => Ok(NuGetVersion {
                major: semver.major,
                minor: semver.minor,
                patch: semver.patch,
                revision: 0,
                prerelease: if semver.pre.is_empty() {
                    None
                } else {
                    Some(semver.pre.to_string())
                },
            }),
            Revision::Opaque(opaque) => {
                let (remaining, version) = NuGetVersion::parse(opaque).map_err(|e| {
                    NuGetConstraintError::VersionParse {
                        version: opaque.clone(),
                        message: format!("Failed to parse NuGet version: {e}"),
                    }
                })?;

                if !remaining.is_empty() {
                    return Err(NuGetConstraintError::VersionParse {
                        version: opaque.clone(),
                        message: format!("Unexpected trailing characters: '{remaining}'"),
                    });
                }

                Ok(version)
            }
        }
    }
}

impl PartialOrd for NuGetVersion {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NuGetVersion {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.major != other.major {
            self.major.cmp(&other.major)
        } else if self.minor != other.minor {
            self.minor.cmp(&other.minor)
        } else if self.patch != other.patch {
            self.patch.cmp(&other.patch)
        } else if self.revision != other.revision {
            self.revision.cmp(&other.revision)
        } else {
            // Treat prereleases as case-insensitive and younger than full releases.
            match (&self.prerelease, &other.prerelease) {
                (None, Some(_)) => Ordering::Greater,
                (Some(_), None) => Ordering::Less,
                (Some(self_pre), Some(other_pre)) => {
                    // Case insensitive comparison for prerelease identifiers
                    self_pre.to_lowercase().cmp(&other_pre.to_lowercase())
                }
                (None, None) => Ordering::Equal,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use simple_test_case::test_case;

    use super::*;
    use crate::{Revision, constraint};

    const FETCHER: Fetcher = Fetcher::Nuget;

    #[test_case("= 1.0.0", vec![constraint!(Equal => "1.0.0")]; "1.0.0_eq_1.0.0")]
    #[test_case(">= 1.0, < 2.0", vec![constraint!(GreaterOrEqual => "1.0"), constraint!(Less => "2.0")]; "1.0_geq_1.0_AND_lt_2.0")]
    #[test_case("> 1.0.0-alpha", vec![constraint!(Greater => "1.0.0-alpha")]; "1.0.0-alpha_gt_1.0.0-alpha")]
    #[test_case("<= 2.0.0", vec![constraint!(LessOrEqual => "2.0.0")]; "2.0.0_leq_2.0.0")]
    #[test_case("= 1.0.0.1", vec![constraint!(Equal => "1.0.0.1")]; "1.0.0.1_eq_1.0.0.1")]
    #[test]
    fn test_nuget_constraints_parsing(input: &str, expected: Vec<Constraint>) {
        let actual = parse_constraints(input).expect("should parse constraint");
        assert_eq!(
            expected, actual,
            "parse_constraints {expected:?} with {actual:?}"
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
    fn test_nuget_constraints_parsing_failure(input: &str) {
        parse_constraints(input).expect_err("should not parse constraint");
    }

    #[test_case(constraint!(Equal => "1.0.0"), Revision::from("1.0.0"), true; "equal_versions")]
    #[test_case(constraint!(Equal => "1.0"), Revision::from("1.0.0"), true; "equal_normalized_versions")]
    #[test_case(constraint!(Equal => "1"), Revision::from("1.0.0"), true; "equal_normalized_versions_major")]
    #[test_case(constraint!(Equal => "1.0.0.0"), Revision::from("1.0"), true; "equal_normalized_versions_with_zeros")]
    #[test_case(constraint!(Equal => "1.0.0"), Revision::from("1.0.0.1"), false; "unequal_with_revision")]
    #[test_case(constraint!(Equal => "1.0.0-alpha"), Revision::from("1.0.0-ALPHA"), true; "equal_case_insensitive_prerelease")]
    #[test_case(constraint!(GreaterOrEqual => "1.0.0"), Revision::from("1.0.0"), true; "greater_equal_same")]
    #[test_case(constraint!(GreaterOrEqual => "1.0.0"), Revision::from("0.9"), false; "not_greater_equal")]
    #[test_case(constraint!(Less => "2.0.0"), Revision::from("1.9.9"), true; "less_than")]
    #[test_case(constraint!(Less => "2.0.0"), Revision::from("1.9.9.9"), true; "less_than_with_revision")]
    #[test_case(constraint!(Less => "2.0.0.1"), Revision::from("1.9.9.9"), true; "less_than_with_revisions")]
    #[test_case(constraint!(Less => "1.0"), Revision::from("1"), false; "not_less_equal")]
    #[test_case(constraint!(Greater => "1.0.0-alpha"), Revision::from("1.0.0"), true; "release_greater_than_prerelease")]
    #[test_case(constraint!(Greater => "1.0.0-alpha"), Revision::from("1.0.0-beta"), true; "beta_greater_than_alpha")]
    #[test_case(constraint!(Equal => "1.0.0+metadata"), Revision::from("1.0.0"), true; "ignore_metadata_in_comparison")]
    #[test]
    fn test_nuget_version_comparison(constraint: Constraint, target: Revision, expected: bool) {
        assert_eq!(
            compare(&constraint, FETCHER, &target).expect("should not have a parse error"),
            expected,
            "compare '{target}' to '{constraint}', expected: {expected}"
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
    #[test_case("7.8.9-alpha", "7.8.9-ALPHA", Ordering::Equal; "case_insensitive_prerelease")]
    #[test]
    fn test_nuget_version_ordering(version1: &str, version2: &str, expected: Ordering) {
        let v1 =
            NuGetVersion::try_from(&Revision::Opaque(version1.to_string())).expect("valid version");
        let v2 =
            NuGetVersion::try_from(&Revision::Opaque(version2.to_string())).expect("valid version");
        assert_eq!(
            v1.cmp(&v2),
            expected,
            "Expected {version1} to be {expected:?} {version2}"
        );
    }
}
