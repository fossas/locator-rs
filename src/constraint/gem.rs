//! Constraints parsing and comparison logic for Rubygems versions.

use std::cmp::Ordering;

use derive_more::Display;
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, digit1, multispace0},
    combinator::{eof, map_res, opt},
    error::{Error as NomError, ErrorKind},
    multi::{many1, separated_list0, separated_list1},
    sequence::{delimited, terminated},
};
use thiserror::Error;

use super::{Comparable, Constraint, Constraints};
use crate::Revision;

/// Gem Requirements and their Comparisons:
///
/// Per https://guides.rubygems.org/patterns/#semantic-versioning, rubygems urges semantic versioning,
/// But does not dictate semantic versioning.
///
/// https://github.com/rubygems/rubygems/blob/master/lib/rubygems/version.rb gives us:
///
/// > If any part contains letters (currently only a-z are supported) then
/// > that version is considered prerelease. Requirements with a prerelease
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
impl Comparable<Revision> for Requirement {
    fn compatible(&self, v: &Revision) -> bool {
        let Ok(target) = Self::from_revision(v) else {
            return Revision::from(self).compatible(v);
        };

        // For ruby-specific semantics of twiddle-wakka compatibility:
        // ~> 2.2.3 is >= 2.2.3, < 2.3.0
        // ~> 2.2 is >= 2.2, < 3.0.0
        // ~> 2 is >= 2, < 3.0.0

        // First, count the non-prerelease segments to determine behavior
        let release_segment_count = self
            .segments
            .iter()
            .take_while(|s| !matches!(s, Segment::Prerelease(_)))
            .count();

        // Create the upper bound by taking the appropriate segments
        let mut stop_segments = match release_segment_count {
            0 => return false, // No release segments, can't determine compatibility
            1 => {
                // ~> 2 => < 3.0.0
                self.segments[0..1].to_vec()
            }
            _ => {
                // ~> 2.2 or ~> 2.2.3 etc.
                // For ~> 2.2, take 1 segment
                // For ~> 2.2.3, take 2 segments
                self.segments[0..release_segment_count.min(2)].to_vec()
            }
        };

        // Increment the last segment for the upper bound
        if let Some(Segment::Release(n)) = stop_segments.last_mut() {
            *n += 1;
        }

        // Add zeros to normalize the versions if needed
        // For example, if stop_segments is [2], make it [2, 0, 0]
        if stop_segments.len() == 1 {
            stop_segments.push(Segment::Release(0));
            stop_segments.push(Segment::Release(0));
        } else if stop_segments.len() == 2 {
            stop_segments.push(Segment::Release(0));
        }

        let stop = Requirement {
            segments: stop_segments,
        };

        // The constraint requires the target to be >= the constraint and < the stop
        self <= &target && target < stop
    }

    fn equal(&self, v: &Revision) -> bool {
        match Self::from_revision(v) {
            Ok(ref target) => self == target,
            Err(_) => Revision::from(self).equal(v),
        }
    }

    fn less(&self, v: &Revision) -> bool {
        match Self::from_revision(v) {
            Ok(ref target) => self < target,
            Err(_) => Revision::from(self).less(v),
        }
    }

    fn greater(&self, v: &Revision) -> bool {
        match Self::from_revision(v) {
            Ok(ref target) => self > target,
            Err(_) => Revision::from(self).greater(v),
        }
    }

    fn not_equal(&self, v: &Revision) -> bool {
        match Self::from_revision(v) {
            Ok(ref target) => self != target,
            Err(_) => Revision::from(self).not_equal(v),
        }
    }

    fn less_or_equal(&self, v: &Revision) -> bool {
        match Self::from_revision(v) {
            Ok(ref target) => self <= target,
            Err(_) => Revision::from(self).less_or_equal(v),
        }
    }

    fn greater_or_equal(&self, v: &Revision) -> bool {
        match Self::from_revision(v) {
            Ok(ref target) => self >= target,
            Err(_) => Revision::from(self).greater_or_equal(v),
        }
    }
}

/// Parse a rubygems requirements string into [`Constraints`].
///
/// See [Gem::Requirement](https://github.com/rubygems/rubygems/blob/master/lib/rubygems/requirement.rb) for more.
pub fn parse(input: &str) -> Result<Constraints<Requirement>, Error> {
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

    fn single_constraint(input: &str) -> IResult<&str, Constraint<Requirement>> {
        let (input, (op_opt, rev)) = (
            delimited(multispace0, opt(operator), multispace0),
            delimited(multispace0, Requirement::parse, multispace0),
        )
            .parse(input)?;

        let op = op_opt.unwrap_or("=");

        let constraint = match op {
            "=" => Constraint::Equal(rev),
            "!=" => Constraint::NotEqual(rev),
            ">" => Constraint::Greater(rev),
            ">=" => Constraint::GreaterOrEqual(rev),
            "<" => Constraint::Less(rev),
            "<=" => Constraint::LessOrEqual(rev),
            "~>" => Constraint::Compatible(rev),
            _ => return Err(nom::Err::Error(NomError::new(input, ErrorKind::Tag))),
        };

        Ok((input, constraint))
    }

    // Parse multiple comma-separated constraints
    fn constraints(input: &str) -> IResult<&str, Vec<Constraint<Requirement>>> {
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
        .map(|(_, parsed)| Constraints::from(parsed))
        .map_err(|e| Error::ParseConstraint {
            constraints: input.to_string(),
            message: format!("failed to parse constraint: {e:?}"),
        })
}

/// Errors from running the gem constraint.
#[derive(Error, Clone, PartialEq, Eq, Debug, Display)]
pub enum Error {
    /// Errors from parsing Gem requirements.
    #[display("RequirementParseError({requirement}, {message})")]
    ParseRequirement {
        /// The failed-to-parse requirement
        requirement: String,

        /// The underlying parse error's message
        message: String,
    },

    /// Errors from parsing Gem constraints.
    #[display("ConstraintsParseError({constraints}, {message})")]
    ParseConstraint {
        /// The failed-to-parse constraints string
        constraints: String,

        /// The underlying parse error's message
        message: String,
    },
}

impl From<semver::Version> for Requirement {
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
        Requirement { segments }
    }
}

impl Requirement {
    /// Creates a new Requirement from a Revision reference.
    ///
    /// This method attempts to convert a Revision to a Requirement by:
    /// - Directly converting SemVer versions
    /// - Parsing opaque strings as Ruby Gems version requirements
    ///
    /// # Returns
    /// - `Ok(Requirement)` if the conversion is successful
    /// - `Err(Error)` if the Revision contains an invalid requirement
    pub fn from_revision(rev: &Revision) -> Result<Self, Error> {
        match rev {
            Revision::Semver(semver) => Ok(Self::from(semver.to_owned())),
            Revision::Opaque(opaque) => Self::parse(opaque.as_str())
                .map_err(|e| Error::ParseRequirement {
                    requirement: opaque.to_string(),
                    message: e.to_string(),
                })
                .and_then(|(leftovers, v)| {
                    if leftovers.is_empty() {
                        Ok(v)
                    } else {
                        Err(Error::ParseRequirement {
                            requirement: opaque.to_string(),
                            message: format!("trailing characters: '{leftovers}'"),
                        })
                    }
                }),
        }
    }
}

/// A Ruby Gems version requirement.
///
/// This structure represents version specifications from Ruby Gems packages,
/// supporting their unique version format with numeric segments and prerelease identifiers.
///
/// Ruby Gems uses a flexible versioning scheme that can represent both SemVer-style versions
/// and Ruby-specific prerelease formats. For example, prereleases with numbers (like `1.2.a3`)
/// expand to multiple segments (`1.2.a.3`) for proper sorting behavior.
///
/// ## Requirement Format
///
/// Ruby Gems versions can include:
/// - Numeric release segments (like `1.2.3`)
/// - Alphabetic prerelease identifiers (like `alpha`, `beta`, `rc`)
/// - Mixed prerelease formats (like `1.0.0.beta2`)
///
/// ## Ordering Rules
///
/// Ruby Gems follows specific version ordering rules:
/// - Release segments are compared numerically
/// - Prerelease segments are compared alphabetically
/// - Prereleases sort lower than full releases
/// - Mixed numeric/alphabetic prereleases are split into separate segments
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Requirement {
    /// The parts of the version. Prereleases with numbers end up in multiple parts (e.g. 1.0.a2 has segments 1, 0, a, and 2).
    segments: Vec<Segment>,
}

impl Requirement {
    /// Parse a rubygems version, as described by https://ruby-doc.org/stdlib-3.0.0/libdoc/rubygems/rdoc/Gem/Requirement.html.
    fn parse(input: &str) -> IResult<&str, Self> {
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
        fn version(input: &str) -> IResult<&str, Requirement> {
            let (input, nested) = separated_list0(tag("."), inner_segments).parse(input)?;
            let segments = nested.into_iter().flatten().collect();
            Ok((input, Requirement { segments }))
        }

        let input = input.trim();
        version(input)
    }
}

impl std::fmt::Display for Requirement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let segments = self
            .segments
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        write!(f, "{}", segments.join("."))
    }
}

impl From<&Requirement> for Revision {
    fn from(v: &Requirement) -> Self {
        Self::Opaque(v.to_string())
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

impl std::fmt::Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Segment::Release(n) => write!(f, "{n}"),
            Segment::Prerelease(s) => write!(f, "{s}"),
        }
    }
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

impl PartialOrd for Requirement {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Requirement {
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
    use crate::{Revision, constraint, constraints};

    macro_rules! segment {
        (rel => $n:literal) => {
            Segment::Release($n)
        };
        (pre => $s:literal) => {
            Segment::Prerelease(String::from($s))
        };
    }

    macro_rules! version {
        ($({ $($segment:tt)* }),* $(,)?) => {
            Requirement { segments: vec![
                $(
                    segment!($($segment)*)
                ),*
            ] }
        };
    }

    #[test_case(">= 1.0.0", constraints!({ GreaterOrEqual => version!({ rel => 1 }, { rel => 0 }, { rel => 0 }) }); "gte_1.0.0")]
    #[test_case("~> 2.5", constraints!({ Compatible => version!({ rel => 2 }, { rel => 5 }) }); "compat_2.5")]
    #[test_case("< 3.0.0", constraints!({ Less => version!({ rel => 3 }, { rel => 0 }, { rel => 0 }) }); "less_3.0.0")]
    #[test_case("= 1.2.3", constraints!({ Equal => version!({ rel => 1 }, { rel => 2 }, { rel => 3 }) }); "eq_1.2.3")]
    #[test_case("!= 1.9.3", constraints!({ NotEqual => version!({ rel => 1 }, { rel => 9 }, { rel => 3 }) }); "notequal_1.9.3")]
    #[test_case("~> 1.0.0.beta2", constraints!({ Compatible => version!({ rel => 1 }, { rel => 0 }, { rel => 0 }, { pre => "beta" }, { rel => 2 }) }); "compat_1.0.0.beta2")]
    #[test_case("= 1.0.0.rc1", constraints!({ Equal => version!({ rel => 1 }, { rel => 0 }, { rel => 0 }, { pre => "rc" }, { rel => 1 }) }); "eq_1.0.0.rc1")]
    #[test_case("~> 3.2.0.rc3", constraints!({ Compatible => version!({ rel => 3 }, { rel => 2 }, { rel => 0 }, { pre => "rc" }, { rel => 3 }) }); "compat_3.2.0.rc3")]
    #[test_case(
        ">= 1.0, < 2.0",
        constraints!(
            { GreaterOrEqual => version!({ rel => 1 }, { rel => 0 }) },
            { Less => version!({ rel => 2 }, {rel => 0 }) },
        );
        "gte_1.0_and_lt_2.0"
    )]
    #[test_case(
        "~> 2.2, >= 2.2.1",
        constraints!(
            { Compatible => version!({ rel => 2 }, { rel => 2 }) },
            { GreaterOrEqual => version!({ rel => 2 }, { rel => 2 }, { rel => 1 }) },
        );
        "compat_2.2_and_gte_2.2.1"
    )]
    #[test_case(
        "> 1.0.0.pre.alpha",
        constraints!(
            { Greater => version!({ rel => 1 }, { rel => 0 }, { rel => 0 }, { pre => "pre" }, { pre => "alpha" }) }
        );
        "gt_1.0.0.pre.alpha"
    )]
    #[test_case(
        ">= 0.8.0, < 1.0.0.beta",
        constraints!(
            { GreaterOrEqual => version!({ rel => 0 }, { rel => 8 }, { rel => 0 }) },
            { Less => version!({ rel => 1 }, { rel => 0 }, { rel => 0 }, { pre => "beta" }) }
        );
        "gte_0.8.0_and_lt_1.0.0.beta"
    )]
    #[test_case(
        ">= 4.0.0.alpha, < 5",
        constraints!(
            { GreaterOrEqual => version!({ rel => 4 }, { rel => 0 }, { rel => 0 }, { pre => "alpha" }) },
            { Less => version!({ rel => 5 }) }
        );
        "gte_4.0.0.alpha_and_lt_5"
    )]
    #[test]
    fn ruby_constraints_parsing(input: &str, expected: Constraints<Requirement>) {
        let actual = parse(input).expect("should parse constraint");
        assert_eq!(expected, actual, "compare {expected:?} with {actual:?}");
    }

    #[test_case("$%!@#"; "invalid_special_chars")]
    #[test_case("1.2.3 !!"; "trailing_invalid_chars")]
    #[test_case("1..2.3"; "double_dot_in_version")]
    #[test_case(">>= 1.0"; "invalid_operator")]
    #[test]
    fn ruby_constraints_parsing_failure(input: &str) {
        parse(input).expect_err("should not parse constraint");
    }

    #[test_case("~> ", constraints!({ Compatible => Requirement { segments: vec![] } }); "missing_version_after_operator")]
    #[test_case(
        ">= 1.0,",
        constraints!(
            { GreaterOrEqual => Requirement { segments: vec![Segment::Release(1), Segment::Release(0)] } },
            { Equal => Requirement { segments: vec![] } }
        );
        "trailing_comma"
    )]
    #[test]
    fn ruby_constraints_parsing_edge_cases(input: &str, expected: Constraints<Requirement>) {
        let actual = parse(input).expect("should parse constraint");
        assert_eq!(actual, expected, "compare {expected:?} with {actual:?}");
    }

    #[test_case(constraint!(Greater => version!({ pre => "b" })), Revision::from("a"), true; "a_not_greater_than_b")]
    #[test_case(constraint!(Compatible => version!({ pre => "abcd" })), Revision::from("AbCd"), false; "abcd_not_compatible_AbCd")]
    #[test_case(constraint!(Compatible => version!({ rel => 1 }, { rel => 2 }, { rel => 3 }, { rel => 4 }, { rel => 5 })), Revision::from("1.2.3.4.5.6"), true; "1.2.3.4.5_compat_1.2.3.4.5.6")]
    #[test_case(constraint!(Compatible => version!({ rel => 1 }, { rel => 2 }, { rel => 3 }, { rel => 4 }, { rel => 5 })), Revision::from("1.2.3.5"), true; "1.2.3.5_not_compat_1.2.3.4.5")]
    #[test_case(constraint!(Compatible => version!({ rel => 1 }, { rel => 2 }, { rel => 0 })), Revision::from("1.2.3"), true; "1.2_compat_1.2.3")]
    #[test_case(constraint!(Compatible => version!({ rel => 1 }, { rel => 2 }, { rel => 0 })), Revision::from("1.3.4"), false; "1.2_compat_1.3.4")]
    #[test_case(constraint!(Compatible => version!({ rel => 1 }, { rel => 2 }, { pre => "a" }, { rel => 0 })), Revision::from("1.2.a0"), true; "1.2.a.0_compat_1.2.a0")]
    #[test_case(constraint!(GreaterOrEqual => version!({ rel => 1 }, { rel => 2 }, { pre => "prerelease0" })), Revision::from("1.2.prerelease1"), true; "1.2.prerelease1_greater_or_equal_1.2prerelease0")]
    #[test_case(constraint!(GreaterOrEqual => version!({ rel => 1 }, { rel => 2 }, { pre => "a" }, { rel => 0 })), Revision::from("1.2.a0"), true; "1.2.a.0_greater_or_equal_1.2a0")]
    #[test_case(constraint!(Equal => version!({ rel => 1 }, { rel => 2 }, { pre => "a" }, { rel => 0 })), Revision::from("1.2.a0"), true; "1.2.a.0_equal_1.2a0")]
    #[test]
    fn compare_ruby_specific(
        constraint: Constraint<Requirement>,
        target: Revision,
        expected: bool,
    ) {
        assert_eq!(
            constraint.matches(&target),
            expected,
            "check if version .{target}. matches constraint .{constraint}. to '{constraint}', expected: {expected}"
        );
    }

    // Testing that we produce the same outputs as our fallback for semvers.
    #[test_case(constraint!(Compatible => version!({ rel => 1 }, { rel => 2 }, { rel => 3 })), Revision::from("1.2.3"); "1.2.3_compatible_1.2.3")]
    #[test_case(constraint!(Equal => version!({ rel => 1 }, { rel => 2 }, { rel => 3 })), Revision::from("1.2.3"); "1.2.3_equal_1.2.3")]
    #[test_case(constraint!(NotEqual => version!({ rel => 1 }, { rel => 2 }, { rel => 3 })), Revision::from("1.2.4"); "1.2.3_not_equal_1.2.3")]
    #[test_case(constraint!(Less => version!({ rel => 1 }, { rel => 2 }, { rel => 3 })), Revision::from("1.2.2"); "1.2.2_less_1.2.3")]
    #[test_case(constraint!(LessOrEqual => version!({ rel => 1 }, { rel => 2 }, { rel => 3 })), Revision::from("1.2.2"); "1.2.2_less_or_equal_1.2.3")]
    #[test_case(constraint!(Greater => version!({ rel => 1 }, { rel => 2 }, { rel => 3 })), Revision::from("1.2.4"); "1.2.4_greater_1.2.3")]
    #[test_case(constraint!(GreaterOrEqual => version!({ rel => 1 }, { rel => 2 }, { rel => 3 })), Revision::from("1.2.2"); "1.2.2_not_greater_or_equal_1.2.3")]
    #[test]
    fn compare_semver_acts_like_fallback(constraint: Constraint<Requirement>, target: Revision) {
        let expected = constraint.map_ref(Revision::from).matches(&target);
        assert_eq!(
            constraint.matches(&target),
            expected,
            "check if version .{target}. matches constraint .{constraint}. to '{constraint}', expected: {expected}",
        );
    }

    #[test_case(constraint!(Equal => version!({ pre => "abcd" })), Revision::from("aBcD"); "abcd_not_equal_aBcD")]
    #[test_case(constraint!(NotEqual => version!({ pre => "abcd" })), Revision::from("abcde"); "abcd_notequal_abcde")]
    #[test_case(constraint!(Less => version!({ pre => "a" })), Revision::from("a"); "a_not_less_a")]
    #[test]
    fn compare_opaque(constraint: Constraint<Requirement>, target: Revision) {
        let expected = constraint.map_ref(Revision::from).matches(&target);
        assert_eq!(
            constraint.matches(&target),
            expected,
            "check if version .{target}. matches constraint .{constraint}. to '{constraint}', expected: {expected}"
        );
    }
}
