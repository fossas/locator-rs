use std::cmp::Ordering;

use derive_more::Display;
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::digit1,
    combinator::map_res,
    multi::{many1, separated_list0},
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
pub fn compare(
    constraint: &Constraint,
    fetcher: Fetcher,
    target: &Revision,
) -> Result<bool, GemVersionParseError> {
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
                    .collect::<Vec<_>>()
                    .clone();
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

/// Errors from parsing Gem versions.
#[derive(Error, Clone, PartialEq, Eq, Debug, Display)]
#[display("GemVersionParseError({version}, {message})")]
pub struct GemVersionParseError {
    /// The failed-to-parse version
    version: String,

    /// The underlying parse error's message
    message: String,
}

impl From<semver::Version> for GemVersion {
    fn from(version: semver::Version) -> Self {
        GemVersion {
            segments: vec![
                Segment::Release(version.major as usize),
                Segment::Release(version.minor as usize),
                Segment::Release(version.patch as usize),
            ],
        }
    }
}

impl TryFrom<&Revision> for GemVersion {
    type Error = GemVersionParseError;

    fn try_from(rev: &Revision) -> Result<Self, Self::Error> {
        match rev {
            Revision::Semver(semver) => Ok(GemVersion::from(semver.to_owned())),
            Revision::Opaque(opaque) => {
                GemVersion::parse(opaque.as_str())
                    .map(|(_, v)| v)
                    .map_err(|e| GemVersionParseError {
                        version: opaque.to_string(),
                        message: e.to_string(),
                    })
            }
        }
    }
}

impl TryFrom<Revision> for GemVersion {
    type Error = GemVersionParseError;

    fn try_from(value: Revision) -> Result<Self, Self::Error> {
        match value {
            Revision::Semver(version) => Ok(version.into()),
            Revision::Opaque(opaque) => match GemVersion::parse(opaque.as_str()) {
                Ok((_, gem_version)) => Ok(gem_version),
                Err(err) => Err(GemVersionParseError {
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
            (Segment::Prerelease(l), Segment::Prerelease(r)) => lexical_sort::lexical_cmp(l, r),
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

    // Using a fetcher that is unlikely to ever have actual comparison functionality.
    const FETCHER: Fetcher = Fetcher::Gem;

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
            compare(&constraint, FETCHER, &target).expect("should not have a parse error"),
            expected,
            "compare '{target}' to '{constraint}', expected: {expected}"
        );
    }

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

    #[test_case(constraint!(GreaterOrEqual => "1.2.3.4"), Revision::from("1.2.3.5"), true; "1.2.3.4_greater_than_1.2.3.5")]
    #[test_case(constraint!(Compatible => "1.2.3.4.5"), Revision::from("1.2.3.4.5.6"), true; "1.2.3.4.5_compat_1.2.3.4.5.6")]
    #[test_case(constraint!(Compatible => "abcd"), Revision::from("AbCd"), false; "abcd_compatible_AbCd")]
    #[test_case(constraint!(Compatible => "abcd"), Revision::from("AbCdE"), false; "abcd_not_compatible_AbCdE")]
    #[test_case(constraint!(Equal => "abcd"), Revision::from("abcd"), true; "abcd_equal_abcd")]
    #[test_case(constraint!(Equal => "abcd"), Revision::from("aBcD"), false; "abcd_not_equal_aBcD")]
    #[test_case(constraint!(NotEqual => "abcd"), Revision::from("abcde"), true; "abcd_notequal_abcde")]
    #[test_case(constraint!(NotEqual => "abcd"), Revision::from("abcd"), false; "abcd_not_notequal_abcd")]
    #[test_case(constraint!(Less => "a"), Revision::from("b"), false; "b_not_less_a")]
    #[test_case(constraint!(Less => "a"), Revision::from("a"), false; "a_not_less_a")]
    #[test_case(constraint!(Greater => "b"), Revision::from("a"), false; "a_not_greater_b")]
    #[test_case(constraint!(Greater => "b"), Revision::from("c"), true; "c_greater_b")]
    #[test_case(constraint!(Less => "あ"), Revision::from("え"), false; "jp_a_not_less_e")]
    #[test_case(constraint!(Greater => "え"), Revision::from("あ"), false; "jp_e_not_greater_a")]
    #[test_case(constraint!(Equal => "あ"), Revision::from("あ"), true; "jp_a_equal_a")]
    #[test_case(constraint!(Compatible => "Maße"), Revision::from("MASSE"), false; "gr_masse_compatible_MASSE")]
    #[test]
    fn compare_opaque(constraint: Constraint, target: Revision, expected: bool) {
        assert_eq!(
            compare(&constraint, FETCHER, &target).expect("should not have a parse error"),
            expected,
            "compare '{target}' to '{constraint}', expected: {expected}"
        );
    }
}
