use std::cmp::Ordering;

use derive_more::Display;
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::digit1,
    character::complete::{char, multispace0},
    combinator::map_res,
    combinator::{opt, recognize},
    multi::separated_list1,
    multi::{many1, separated_list0},
    sequence::{delimited, pair},
};
use thiserror::Error;

use super::{Constraint, fallback};
use crate::{Fetcher, Revision};

/// Gem Versions and their Comparisons:
///
/// Per https://guides.rubygems.org/patterns/#semantic-versioning, rubygems urges semantic versioning,
/// But does not dictate semantic versioning.
///
/// https://github.com/rubygems/rubygems/blob/master/lib/rubygems/version.rb gives us:
///
/// > If any part contains letters (currently only a-z are supported) then
/// > that version is considered prerelease. Versions with a prerelease
/// > part in the Nth part sort less than versions with N-1
/// > parts. Prerelease parts are sorted alphabetically using the normal
/// > Ruby string sorting rules. If a prerelease part contains both
/// > letters and numbers, it will be broken into multiple parts to
/// > provide expected sort behavior (1.0.a10 becomes 1.0.a.10, and is
/// > greater than 1.0.a9).
/// >
/// > Prereleases sort between real releases (newest to oldest):
/// >
/// > 1. 1.0
/// > 2. 1.0.b1
/// > 3. 1.0.a.2
/// > 4. 0.9
/// >
/// > If you want to specify a version restriction that includes both prereleases
/// > and regular releases of the 1.x series this is the best way:
/// >
/// >   s.add_dependency 'example', '>= 1.0.0.a', '< 2.0.0'
///
/// An Advisory Example: ruby-advisory-db/gems/activesupport/CVE-2009-3009.yml
///
/// ```yaml
/// unaffected_versions:
///   - "< 2.0.0"
/// patched_versions:
///   - "~> 2.2.3"
///   - ">= 2.3.4"
/// ```
///
/// Note the "twiddle-wakka" syntax for pessimistic version matching.
/// See here: https://guides.rubygems.org/patterns/#pessimistic-version-constraint
/// So `~> 2.2.3` is equivalent to `>= 2.2.3,< 2.3.0`.
/// Leaving out the patch, `~> 2.2` is equivalent to `>= 2.2.0,<3.0.0`.
/// Finally, `~> 2` is the closest to ~=/Compatible, and is equivalent to `>= 2.0.0,<3.0.0`.
#[tracing::instrument]
pub fn compare(
    constraint: &Constraint,
    fetcher: Fetcher,
    target: &Revision,
) -> Result<bool, GemConstraintError> {
    if let (Revision::Semver(_), Revision::Semver(_)) = (constraint.revision(), target) {
        Ok(fallback::compare(constraint, fetcher, target))
    } else {
        let threshold = GemVersion::try_from(constraint.revision())?;
        let target = GemVersion::try_from(target)?;
        Ok(match constraint {
            Constraint::Equal(_) => target == threshold,
            Constraint::NotEqual(_) => target != threshold,
            Constraint::Less(_) => target < threshold,
            Constraint::LessOrEqual(_) => target <= threshold,
            Constraint::Greater(_) => target > threshold,
            Constraint::GreaterOrEqual(_) => target >= threshold,
            Constraint::Compatible(_) => {
                let mut stop_segments = threshold
                    .segments
                    .iter()
                    .take_while(|s| !matches!(s, Segment::Prerelease(_)))
                    .take(threshold.segments.len() - 1)
                    .cloned()
                    .collect::<Vec<_>>();
                if let Some(Segment::Release(n)) = stop_segments.last_mut() {
                    *n += 1;
                }
                let stop = GemVersion {
                    segments: stop_segments,
                };
                target >= threshold && target < stop
            }
        })
    }
}

/// Parsing rubygems requirements:
/// Parses a string into a vector of constraints.
/// See [Gem::Requirement](https://github.com/rubygems/rubygems/blob/master/lib/rubygems/requirement.rb) for more.
pub fn parse_constraints(input: &str) -> Result<Vec<Constraint>, GemConstraintError> {
    fn operator(input: &str) -> IResult<&str, &str> {
        alt((
            tag("="),
            tag("!="),
            tag(">="),
            tag("<="),
            tag("~>"),
            tag(">"),
            tag("<"),
        ))
        .parse(input)
    }

    fn version_segment(input: &str) -> IResult<&str, &str> {
        take_while1(|c: char| c.is_alphanumeric())(input)
    }

    fn version(input: &str) -> IResult<&str, &str> {
        recognize(pair(
            version_segment,
            opt(recognize(pair(
                char('.'),
                separated_list0(char('.'), version_segment),
            ))),
        ))
        .parse(input)
    }

    fn single_constraint(input: &str) -> IResult<&str, Constraint> {
        let (input, (op_opt, ver)) = (
            delimited(multispace0, opt(operator), multispace0),
            delimited(multispace0, version, multispace0),
        )
            .parse(input)?;

        let op = op_opt.unwrap_or("=");
        let rev = Revision::Opaque(ver.to_string());

        let constraint = match op {
            "=" => Constraint::Equal(rev),
            "!=" => Constraint::NotEqual(rev),
            ">" => Constraint::Greater(rev),
            ">=" => Constraint::GreaterOrEqual(rev),
            "<" => Constraint::Less(rev),
            "<=" => Constraint::LessOrEqual(rev),
            "~>" => Constraint::Compatible(rev),
            _ => Constraint::Equal(rev), // Default to equality
        };

        Ok((input, constraint))
    }

    // Parse multiple comma-separated constraints
    fn constraints(input: &str) -> IResult<&str, Vec<Constraint>> {
        separated_list1(
            delimited(multispace0, char(','), multispace0),
            single_constraint,
        )
        .parse(input)
    }

    if input.trim().is_empty() {
        return Err(GemConstraintError::VersionParseError {
            version: input.to_string(),
            message: format!("empty input: '{input}'"),
        });
    }

    match constraints(input.trim()) {
        Ok((remaining, parsed_constraints)) => {
            if !remaining.is_empty() {
                return Err(GemConstraintError::VersionParseError {
                    version: input.to_string(),
                    message: format!("trailing text: '{remaining}'"),
                });
            }
            Ok(parsed_constraints)
        }
        Err(e) => Err(GemConstraintError::VersionParseError {
            version: input.to_string(),
            message: format!("failed to parse constraint: {e:?}"),
        }),
    }
}

/// Errors from running the gem constraint.
#[derive(Error, Clone, PartialEq, Eq, Debug, Display)]
pub enum GemConstraintError {
    /// Errors from parsing Gem versions.
    #[display("VersionParseError({version}, {message})")]
    VersionParseError {
        /// The failed-to-parse version
        version: String,

        /// The underlying parse error's message
        message: String,
    },
}

impl From<semver::Version> for GemVersion {
    fn from(version: semver::Version) -> Self {
        let segments = if version.pre.is_empty() {
            vec![
                Segment::Release(version.major as usize),
                Segment::Release(version.minor as usize),
                Segment::Release(version.patch as usize),
            ]
        } else {
            vec![
                Segment::Release(version.major as usize),
                Segment::Release(version.minor as usize),
                Segment::Release(version.patch as usize),
                Segment::Prerelease(version.pre.to_string()),
            ]
        };
        GemVersion { segments }
    }
}

impl TryFrom<&Revision> for GemVersion {
    type Error = GemConstraintError;

    fn try_from(rev: &Revision) -> Result<Self, Self::Error> {
        match rev {
            Revision::Semver(semver) => Ok(GemVersion::from(semver.to_owned())),
            Revision::Opaque(opaque) => GemVersion::parse(opaque.as_str())
                .map_err(|e| GemConstraintError::VersionParseError {
                    version: opaque.to_string(),
                    message: e.to_string(),
                })
                .and_then(|(leftovers, v)| {
                    if leftovers.is_empty() {
                        Ok(v)
                    } else {
                        Err(GemConstraintError::VersionParseError {
                            version: opaque.to_string(),
                            message: format!("trailing characters: '{leftovers}'"),
                        })
                    }
                }),
        }
    }
}

impl TryFrom<Revision> for GemVersion {
    type Error = GemConstraintError;

    fn try_from(value: Revision) -> Result<Self, Self::Error> {
        match value {
            Revision::Semver(version) => Ok(version.into()),
            Revision::Opaque(opaque) => match GemVersion::parse(opaque.as_str()) {
                Ok((_, gem_version)) => Ok(gem_version),
                Err(err) => Err(GemConstraintError::VersionParseError {
                    version: opaque.to_string(),
                    message: err.to_string(),
                }),
            },
        }
    }
}

/// A gem/bundler version.
/// Usually but not always a sem-ver version.
/// Can also include forms like `1.2.prerelease1`, which expand to `1.2.prerelease.1` and indicate prerelease versions.
#[derive(Debug, PartialEq, Eq, Clone)]
struct GemVersion {
    /// The parts of the version. Prereleases with numbers end up in multiple parts (e.g. 1.0.a2 has segments 1, 0, a, and 2).
    segments: Vec<Segment>,
}

impl GemVersion {
    /// Parse a rubygems version, as described by https://ruby-doc.org/stdlib-3.0.0/libdoc/rubygems/rdoc/Gem/Version.html.
    pub fn parse(input: &str) -> IResult<&str, Self> {
        // Usually a gem version is of the familiar form `major.minor.patch`.
        // Sometimes it includes further nesting or prerelease strings a la the examples `1.0.a2` or `0.9.b.0`.

        /// Parses a numeric release segment.
        fn release(input: &str) -> IResult<&str, Segment> {
            map_res(digit1, |s: &str| s.parse::<usize>().map(Segment::Release)).parse(input)
        }

        /// Parses an alphabetic pre-release segment.
        fn prerelease(input: &str) -> IResult<&str, Segment> {
            map_res(
                take_while1(|c: char| c.is_alphabetic()),
                |s: &str| -> std::result::Result<Segment, std::convert::Infallible> {
                    Ok(Segment::Prerelease(s.to_string()))
                },
            )
            .parse(input)
        }

        /// Parses either kind of segment.
        fn segment(input: &str) -> IResult<&str, Segment> {
            alt((release, prerelease)).parse(input)
        }

        /// Since segments are not reliably "."-delimted, this parses the inner segments of a verison (`a2` becomes 'a, then 2', for example.)
        fn inner_segments(input: &str) -> IResult<&str, Vec<Segment>> {
            many1(segment).parse(input)
        }

        /// Parses the segments of a gem version and flattens the result.
        fn version(input: &str) -> IResult<&str, GemVersion> {
            let (input, nested) = separated_list0(tag("."), inner_segments).parse(input)?;
            let segments = nested.into_iter().flatten().collect();
            Ok((input, GemVersion { segments }))
        }

        let input = input.trim();
        version(input)
    }
}

/// Either a number or a prerelease-string
#[derive(Clone, Debug, PartialEq, Eq)]
enum Segment {
    /// A normal release. The '1' or '0' in '1.0.a2'.
    Release(usize),

    /// A pre-release. The 'a' in '1.0.a2'.
    Prerelease(String),
}

impl PartialOrd for Segment {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Segment {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Segment::Release(l), Segment::Release(r)) => l.cmp(r),
            (Segment::Prerelease(l), Segment::Prerelease(r)) => {
                dbg!(l, r);
                lexical_sort::lexical_cmp(l, r)
            }
            (Segment::Prerelease(_), Segment::Release(_)) => Ordering::Less,
            (Segment::Release(_), Segment::Prerelease(_)) => Ordering::Greater,
        }
    }
}

impl PartialOrd for GemVersion {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for GemVersion {
    fn cmp(&self, other: &Self) -> Ordering {
        let max_len = self.segments.len().max(other.segments.len());
        for i in 0..max_len {
            let l = self.segments.get(i);
            let r = other.segments.get(i);
            match (l, r) {
                (Some(l), Some(r)) => {
                    let cmp = l.cmp(r);
                    if cmp != Ordering::Equal {
                        return cmp;
                    }
                }
                (Some(_), None) => return Ordering::Greater,
                (None, Some(_)) => return Ordering::Less,
                (None, None) => return Ordering::Equal,
            }
        }
        Ordering::Equal
    }
}

#[cfg(test)]
mod tests {
    use simple_test_case::test_case;

    use super::*;
    use crate::{Revision, constraint};

    const FETCHER: Fetcher = Fetcher::Gem;

    #[test_case(">= 1.0.0", vec![constraint!(GreaterOrEqual => "1.0.0")]; "1.0.0_geq_1.0.0")]
    #[test_case("~> 2.5", vec![constraint!(Compatible => "2.5")]; "2.5_compat_2.5")]
    #[test_case("< 3.0.0", vec![constraint!(Less => "3.0.0")]; "3.0.0_lt_3.0.0")]
    #[test_case(">= 1.0, < 2.0", vec![constraint!(GreaterOrEqual => "1.0"), constraint!(Less => "2.0")]; "1.0_geq_1.0_AND_lt_2.0")]
    #[test_case("= 1.2.3", vec![constraint!(Equal => "1.2.3")]; "1.2.3_eq_1.2.3")]
    #[test_case("!= 1.9.3", vec![constraint!(NotEqual => "1.9.3")]; "1.9.3_neq_1.9.3")]
    #[test_case("~> 2.2, >= 2.2.1", vec![constraint!(Compatible => "2.2"), constraint!(GreaterOrEqual => "2.2.1")]; "2.2_compat_2.2_AND_geq_2.2.1")]
    #[test_case("> 1.0.0.pre.alpha", vec![constraint!(Greater => "1.0.0.pre.alpha")]; "1.0.0.pre.alpha_gt_1.0.0.pre.alpha")]
    #[test_case("~> 1.0.0.beta2", vec![constraint!(Compatible => "1.0.0.beta2")]; "1.0.0.beta2_compat_1.0.0.beta2")]
    #[test_case("= 1.0.0.rc1", vec![constraint!(Equal => "1.0.0.rc1")]; "1.0.0.rc1_eq_1.0.0.rc1")]
    #[test_case(">= 0.8.0, < 1.0.0.beta", vec![constraint!(GreaterOrEqual => "0.8.0"), constraint!(Less => "1.0.0.beta")]; "0.8.0_geq_0.8.0_AND_lt_1.0.0.beta")]
    #[test_case("~> 3.2.0.rc3", vec![constraint!(Compatible => "3.2.0.rc3")]; "3.2.0.rc3_compat_3.2.0.rc3")]
    #[test_case(">= 4.0.0.alpha, < 5", vec![constraint!(GreaterOrEqual => "4.0.0.alpha"), constraint!(Less => "5")]; "4.0.0.alpha_geq_4.0.0.alpha_AND_lt_5")]
    #[test]
    fn test_ruby_constraints_parsing(input: &str, expected: Vec<Constraint>) {
        let actual = parse_constraints(input).expect("should parse constraint");
        assert_eq!(expected, actual, "compare {expected:?} with {actual:?}");
    }

    #[test_case("$%!@#"; "invalid_special_chars")]
    #[test_case("1.2.3 !!"; "trailing_invalid_chars")]
    #[test_case("1..2.3"; "double_dot_in_version")]
    #[test_case(">>= 1.0"; "invalid_operator")]
    #[test_case("~> "; "missing_version_after_operator")]
    #[test_case(">= 1.0,"; "trailing_comma")]
    #[test]
    fn test_ruby_constraints_parsing_failure(input: &str) {
        parse_constraints(input).expect_err("should not parse constraint");
    }

    #[test_case(constraint!(Greater => "b"), Revision::from("a"), false; "a_not_greater_than_b")]
    #[test_case(constraint!(Compatible => "abcd"), Revision::from("AbCd"), false; "abcd_not_compatible_AbCd")]
    #[test_case(constraint!(GreaterOrEqual => "1.2.3.4"), Revision::from("1.2.3.5"), true; "1.2.3.4_greater_than_1.2.3.5")]
    #[test_case(constraint!(Compatible => "1.2.3.4.5"), Revision::from("1.2.3.4.5.6"), true; "1.2.3.4.5_compat_1.2.3.4.5.6")]
    #[test_case(constraint!(Compatible => "1.2.3.4.5"), Revision::from("1.2.3.5"), false; "1.2.3.5_not_compat_1.2.3.4.5")]
    #[test_case(constraint!(Compatible => 1, 2, 0), Revision::from("1.2.3"), true; "1.2_compat_1.2.3")]
    #[test_case(constraint!(Compatible => 1, 2, 0), Revision::from("1.3.4"), false; "1.2_compat_1.3.4")]
    #[test_case(constraint!(Compatible => "1.2.a.0"), Revision::from("1.2.a0"), true; "1.2.a.0_compat_1.2.a0")]
    #[test_case(constraint!(GreaterOrEqual => "1.2.prerelease0"), Revision::from("1.2.prerelease1"), true; "1.2.prerelease1_greater_or_equal_1.2prerelease0")]
    #[test_case(constraint!(GreaterOrEqual => "1.2.3.4"), Revision::from("1.2.3.5"), true; "1.2.3.5_greater_or_equal_1.2.3.4")]
    #[test_case(constraint!(GreaterOrEqual => "1.2.a.0"), Revision::from("1.2.a0"), true; "1.2.a.0_greater_or_equal_1.2a0")]
    #[test_case(constraint!(Equal => "1.2.a.0"), Revision::from("1.2.a0"), true; "1.2.a.0_equal_1.2a0")]
    #[test]
    fn compare_ruby_specific_oddities(constraint: Constraint, target: Revision, expected: bool) {
        assert_eq!(
            compare(&constraint, FETCHER, &target).expect("should not have a parse error"),
            expected,
            "compare '{target}' to '{constraint}', expected: {expected}"
        );
    }

    #[test_case(constraint!(Equal => 1,2,3), Revision::from("1*"); "1.2.3_equal_1*")]
    #[test_case(constraint!(Equal => "1.2.a.0"), Revision::from("1.2*a0"); "1.2.a.0_equal_1.2*a0")]
    #[test_case(constraint!(Equal => 1,2,3), Revision::from("1*2*3"); "1.2.3_equal_1*2*3")]
    #[test]
    fn compare_ruby_error_cases(constraint: Constraint, target: Revision) {
        assert!(compare(&constraint, FETCHER, &target).is_err())
    }

    // Testing that we produce the same outputs as our fallback for semvers.
    #[test_case(constraint!(Compatible => 1, 2, 3), Revision::from("1.2.3"); "1.2.3_compatible_1.2.3")]
    #[test_case(constraint!(Equal => 1, 2, 3), Revision::from("1.2.4"); "1.2.4_not_equal_1.2.3")]
    #[test_case(constraint!(NotEqual => 1, 2, 3), Revision::from("1.2.3"); "1.2.3_not_notequal_1.2.3")]
    #[test_case(constraint!(Less => 1, 2, 3), Revision::from("1.2.2"); "1.2.2_less_1.2.3")]
    #[test_case(constraint!(LessOrEqual => 1, 2, 3), Revision::from("1.2.2"); "1.2.2_less_or_equal_1.2.3")]
    #[test_case(constraint!(Greater => 1, 2, 3), Revision::from("1.2.4"); "1.2.4_greater_1.2.3")]
    #[test_case(constraint!(GreaterOrEqual => 1, 2, 3), Revision::from("1.2.2"); "1.2.2_not_greater_or_equal_1.2.3")]
    #[test]
    fn compare_semver_acts_like_fallback(constraint: Constraint, target: Revision) {
        let expected = fallback::compare(&constraint, FETCHER, &target);
        assert_eq!(
            compare(&constraint, FETCHER, &target).expect("should not have a parse error"),
            expected,
            "compare '{target}' to '{constraint}', expected: {expected}",
        );
    }

    #[test_case(constraint!(Equal => "abcd"), Revision::from("aBcD"); "abcd_not_equal_aBcD")]
    #[test_case(constraint!(NotEqual => "abcd"), Revision::from("abcde"); "abcd_notequal_abcde")]
    #[test_case(constraint!(Less => "a"), Revision::from("a"); "a_not_less_a")]
    #[test]
    fn compare_opaque(constraint: Constraint, target: Revision) {
        let expected = fallback::compare(&constraint, FETCHER, &target);
        assert_eq!(
            compare(&constraint, FETCHER, &target).expect("should not have a parse error"),
            expected,
            "compare '{target}' to '{constraint}', expected: {expected}"
        );
    }
}
