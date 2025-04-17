//! Pypi Package Version Specifiers
//! See:
//! - [Version Specifiers](https://packaging.python.org/en/latest/specifications/version-specifiers/#version-specifiers)
//! - [Requirement Specifiers](https://pip.pypa.io/en/stable/reference/requirement-specifiers/#requirement-specifiers)
//!
//! Version Specifiers in pypi:
//!
//! `[N!]N(.N)*[{a|b|rc}N][.postN][.devN]`
//!
//! `1!2.3.4a5.post6.dev7`
//!
//! And are made up of these five segments:
//! - Epoch segment: N!
//! - Release segment: N(.N)*
//! - Pre-release segment: {a|b|rc}N
//! - Post-release segment: .postN
//! - Development release segment: .devN
//!
//!
//! The Epoch `N!` defaults to 0, and is rare.
//! It works to separate different regimes of versioning within a project.
//! For example, going from date-based versioning to semantic versioning.
//! `2025.01.01` from January might be older than:
//! `1.0.0`, when the project maintainers decided to switch to semantic versioning.
//!
//! But the `1.0.0` version will look older with the default epoch.
//! So, `1!1.0.0` serves to show that the new epoch has arrived,
//! and all subsequent versions will be considered newer than epoch 0 versions.
//!
//!
//! Release segments `N(.N)*` the common portion of a version.
//! A version identifier that consists solely of a release segment and optionally an epoch identifier is termed a "final release".
//! These are the most commonly thought of part of the version specifier.
//!
//!
//! Pre-release segment: {a|b|rc}N
//! - `a` is for alpha
//! - `b` is for beta
//! - `rc` is for release candidate
//! - `c` is for release candidate, also.
//!
//! Prereleases are all considered older than final releases.
//!
//! Post-release segment: .postN
//! - `post` is for post-release
//! - the default post-release is `.post0`
//!
//! Naturally, post-releases are considered newer than the corresponding final releases.
//!
//!
//! Development release segment: .devN
//! - `dev` is for development release
//! - the default dev-release is `.dev0`
//!
//! Development releases are ordered by their numeric component.
//! They are newer than the post-releases of the prior version.
//! They are older than the pre-releases of their release segment's version.
//!
//!
//! Normalizing version specifiers:
//! - Version specifiers should treat versions as case-insensitive.
//!
//! - Numeric segments should be normalized as a python int (leading zeroes removed, for example)
//!
//! - Prereleases may have a `.`, `-`, or `_` between the release segment and the pre-release segment.
//! - alpha, beta, c, pre, and preview are all considered pre-release segments, and map onto a, b, rc, rc, and rc respectively.
//!
//! - post-releases may also have the same separators that pre-releases may (or none at all)
//! - post-releases may also be speleld `r` or `rev` instead of `post`
//!
//! - dev-releases may also have the same separators that pre-releases may (or none at all)
//!
//! - Any of these versions can begin with a `v`, which is meaningless.
//!
use std::cmp::Ordering;

use derive_more::Display;
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, digit1, multispace0, u32},
    combinator::{eof, map_res, opt, value},
    multi::{many1, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
};
use thiserror::Error;
use tracing::warn;

use super::{Constraint, fallback};
use crate::{Fetcher, Revision};

/// A version in pip.
/// See [Version Specifiers](https://packaging.python.org/en/latest/specifications/version-specifiers/#version-specifiers) for more information.
#[derive(Debug, Clone, PartialEq, Eq)]
struct PipVersion {
    /// The epoch of the version; most versions have an epoch of 0, which is the default.
    /// After, for example, changing from a `yyyy.mm.dd` version format to a semver format, a project might increment its epoch to capture the change.
    /// That would allow the project to indicate that every semver version was later than any date-marked version.
    /// In a version `0!1.2.3.rc4.post5.dev6`, the epoch is 0.
    epoch: u32,

    /// The release segments of the version.
    /// These are what we most commonly picture when we imagine a version.
    /// For example, in the version `1.2.3`, the release segments are `[1, 2, 3]`.
    /// In a version `0!1.2.3.rc4.post5.dev6`, the release segments are `[1, 2, 3]`.
    release_segments: Vec<u32>,

    /// An optional prerelease.
    /// In a version `0!1.2.3.rc4.post5.dev6`, the prerelease is `rc 4`.
    /// In a version `0!1.2.3.pre4.post5.dev6`, the prerelease is `rc 4`.
    /// In a version `0!1.2.3.prerelease4.post5.dev6`, the prerelease is `rc 4`.
    /// In a version `0!1.2.3.a4.post5.dev6`, the prerelease is `a 4`.
    /// In a version `0!1.2.3.b4.post5.dev6`, the prerelease is `b 4`.
    /// In a version `0!1.2.3.c4.post5.dev6`, the prerelease is `rc 4`.
    pre_release: Option<PreRelease>,

    /// An optional sub-patch post-release revision.
    /// In a version `0!1.2.3.rc4.post5.dev6`, the post release is `5`.
    /// In a version `0!1.2.3.rc4.r5.dev6`, the post release is `5`.
    /// In a version `0!1.2.3.rc4.rev5.dev6`, the post release is `5`.
    post_release: Option<u32>,

    /// An optional dev-release, 0 by default.
    /// Dev releases are newer than the post-releases of the prior version and older than pre-releases of their version.
    /// In a version `0!1.2.3.rc4.post5.dev6`, the dev release is `6`.
    dev_release: Option<u32>,
}

#[derive(Error, Clone, PartialEq, Eq, Debug, Display)]
pub enum PipConstraintError {
    #[display("ConstraintParseError({constraints}, {message})")]
    ConstraintParseError {
        constraints: String,
        message: String,
    },

    #[display("VersionParseError({version}, {message})")]
    VersionParseError { version: String, message: String },
}

#[tracing::instrument]
pub fn compare(
    constraint: &Constraint,
    fetcher: Fetcher,
    target: &Revision,
) -> Result<bool, PipConstraintError> {
    if let (Revision::Semver(_), Revision::Semver(_)) = (constraint.revision(), target) {
        Ok(fallback::compare(constraint, fetcher, target))
    } else {
        let threshold = PipVersion::try_from(constraint.revision())?;
        let target = PipVersion::try_from(target)?;
        Ok(match constraint {
            Constraint::Equal(_) => target == threshold,
            Constraint::NotEqual(_) => target != threshold,
            Constraint::Less(_) => target < threshold,
            Constraint::LessOrEqual(_) => target <= threshold,
            Constraint::Greater(_) => target > threshold,
            Constraint::GreaterOrEqual(_) => target >= threshold,
            Constraint::Compatible(_) => {
                let threshold_segments = threshold.release_segments.clone();
                if threshold_segments.len() >= 2 {
                    let min_version = threshold.clone();

                    // Create a max version with one segment incremented
                    // For ~= 2.5, the max version would be < 3.0
                    // For ~= 2.5.1, the max version would be < 2.6.0
                    // For ~= 1!2.5.1, the max version would be < 1!2.6.0
                    let mut max_segments = threshold_segments.clone();
                    // Drop the last element if we have more than 2 segments
                    if max_segments.len() > 2 {
                        max_segments.truncate(max_segments.len() - 1);
                    }
                    // Increment the last remaining segment
                    if let Some(last) = max_segments.last_mut() {
                        *last += 1;
                    }

                    // Create a max version with all zeros after the incremented segment
                    let max_version = PipVersion {
                        epoch: threshold.epoch,
                        release_segments: max_segments,
                        pre_release: None,
                        post_release: None,
                        dev_release: None,
                    };

                    target >= min_version && target < max_version
                } else {
                    warn!("Not enough release segments for compatible operator with {threshold}");
                    false
                }
            }
        })
    }
}

#[tracing::instrument]
pub fn parse_constraints(input: &str) -> Result<Vec<Constraint>, PipConstraintError> {
    fn operator(input: &str) -> IResult<&str, &str> {
        alt((
            tag("==="),
            tag("=="),
            tag("!="),
            tag(">="),
            tag("<="),
            tag("~="),
            tag(">"),
            tag("<"),
        ))
        .parse(input)
    }

    fn version(input: &str) -> IResult<&str, &str> {
        take_while1(|c: char| c.is_alphanumeric() || ".!+-_*".contains(c)).parse(input)
    }

    fn single_constraint(input: &str) -> IResult<&str, Constraint> {
        let (input, (op, ver)) = pair(
            delimited(multispace0, operator, multispace0),
            delimited(multispace0, version, multispace0),
        )
        .parse(input)?;

        let rev = Revision::Opaque(ver.to_string());

        let constraint = match op {
            // Technically, the `===` operator ought to exclude default values.
            "==" | "===" => Constraint::Equal(rev),
            "!=" => Constraint::NotEqual(rev),
            ">" => Constraint::Greater(rev),
            ">=" => Constraint::GreaterOrEqual(rev),
            "<" => Constraint::Less(rev),
            "<=" => Constraint::LessOrEqual(rev),
            "~=" => Constraint::Compatible(rev),
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
        .map_err(|e| PipConstraintError::ConstraintParseError {
            constraints: input.to_string(),
            message: format!("failed to parse constraint: {e:?}"),
        })
}

impl std::fmt::Display for PipVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}!", self.epoch)?;
        let mut is_first = true;
        for segment in &self.release_segments {
            if is_first {
                is_first = false;
                write!(f, "{segment}")?;
            } else {
                write!(f, ".{segment}")?;
            }
        }
        if let Some(pre_release) = &self.pre_release {
            write!(f, "{}", pre_release)?;
        }
        if let Some(post_release) = &self.post_release {
            write!(f, ".post{}", post_release)?;
        }
        if let Some(dev_release) = &self.dev_release {
            write!(f, ".dev{}", dev_release)?;
        }
        Ok(())
    }
}

/// The prerelease version.
/// `{a|alpha|b|beta|rc|c|pre|preview}N`
/// N defaults to 0 in the presence of any of these tags.
#[derive(Debug, Clone, PartialEq, Eq)]
enum PreRelease {
    /// aN | alphaN
    Alpha(u32),

    /// bN |betaN
    Beta(u32),

    /// rcN | cN | preN | previewN
    RC(u32),
}

impl std::fmt::Display for PreRelease {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PreRelease::Alpha(n) => write!(f, "a{}", n),
            PreRelease::Beta(n) => write!(f, "b{}", n),
            PreRelease::RC(n) => write!(f, "rc{}", n),
        }
    }
}

impl PreRelease {
    fn number(&self) -> u32 {
        match self {
            PreRelease::Alpha(n) => *n,
            PreRelease::Beta(n) => *n,
            PreRelease::RC(n) => *n,
        }
    }

    fn parse(input: &str) -> IResult<&str, Self> {
        fn alpha(input: &str) -> IResult<&str, PreRelease> {
            let (input, _) = alt((tag("alpha"), tag("a"))).parse(input)?;
            let (input, number) = opt(u32).parse(input)?;
            Ok((input, PreRelease::Alpha(number.unwrap_or(0))))
        }
        fn beta(input: &str) -> IResult<&str, PreRelease> {
            let (input, _) = alt((tag("beta"), tag("b"))).parse(input)?;
            let (input, number) = opt(u32).parse(input)?;
            Ok((input, PreRelease::Beta(number.unwrap_or(0))))
        }
        fn rc(input: &str) -> IResult<&str, PreRelease> {
            let (input, _) = alt((tag("preview"), tag("rc"), tag("c"), tag("pre"))).parse(input)?;
            let (input, number) = opt(u32).parse(input)?;
            Ok((input, PreRelease::RC(number.unwrap_or(0))))
        }
        alt((alpha, beta, rc)).parse(input)
    }
}

impl PartialOrd for PreRelease {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PreRelease {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (PreRelease::Alpha(_), PreRelease::Beta(_)) => Ordering::Less,
            (PreRelease::Alpha(_), PreRelease::RC(_)) => Ordering::Less,
            (PreRelease::Beta(_), PreRelease::Alpha(_)) => Ordering::Greater,
            (PreRelease::Beta(_), PreRelease::RC(_)) => Ordering::Less,
            (PreRelease::RC(_), PreRelease::Alpha(_)) => Ordering::Greater,
            (PreRelease::RC(_), PreRelease::Beta(_)) => Ordering::Greater,
            (lhs, rhs) => lhs.number().cmp(&rhs.number()),
        }
    }
}

impl TryFrom<&Revision> for PipVersion {
    type Error = PipConstraintError;

    fn try_from(rev: &Revision) -> Result<Self, Self::Error> {
        match rev {
            Revision::Semver(semver) => {
                let pre_release = if semver.pre.is_empty() {
                    None
                } else {
                    PreRelease::parse(semver.pre.as_str())
                        .map(|(_, pre)| Some(pre))
                        .map_err(|e| PipConstraintError::VersionParseError {
                            version: semver.to_string(),
                            message: format!("invalid pre-release version: {e:?}"),
                        })?
                };
                let mut version = PipVersion {
                    epoch: 0,
                    release_segments: vec![
                        semver.major as u32,
                        semver.minor as u32,
                        semver.patch as u32,
                    ],
                    pre_release,
                    post_release: None,
                    dev_release: None,
                };

                let pre_opt = &semver.pre;
                if !pre_opt.is_empty() {
                    if let Ok((_, pre_release)) = PreRelease::parse(pre_opt.as_str()) {
                        version.pre_release = Some(pre_release);
                    } else {
                        return Err(PipConstraintError::VersionParseError {
                            version: semver.to_string(),
                            message: format!("Unexpected pre-release: {}", pre_opt),
                        });
                    }
                }

                Ok(version)
            }
            Revision::Opaque(opaque) => {
                PipVersion::parse(opaque).map_err(|e| PipConstraintError::VersionParseError {
                    version: opaque.to_string(),
                    message: e.to_string(),
                })
            }
        }
    }
}

impl PipVersion {
    /// Parses and normalizes a pip version
    fn parse(version: &str) -> Result<Self, String> {
        fn separator(input: &str) -> IResult<&str, char> {
            alt((char('.'), char('-'), char('_'), value('_', tag("")))).parse(input)
        }

        fn v_prefix(input: &str) -> IResult<&str, ()> {
            value((), opt(tag("v"))).parse(input)
        }

        fn epoch(input: &str) -> IResult<&str, u32> {
            let (input, num) = map_res(digit1, |s: &str| s.parse::<u32>()).parse(input)?;
            let (input, _) = char('!')(input)?;
            Ok((input, num))
        }

        fn release_segment(input: &str) -> IResult<&str, u32> {
            map_res(digit1, |s: &str| s.parse::<u32>()).parse(input)
        }

        fn release_segments(input: &str) -> IResult<&str, Vec<u32>> {
            let (input, first) = release_segment(input)?;
            let (input, rest) = many1(preceded(char('.'), release_segment)).parse(input)?;

            let mut segments = vec![first];
            segments.extend(rest);
            Ok((input, segments))
        }

        fn pre_release(input: &str) -> IResult<&str, PreRelease> {
            preceded(opt(separator), PreRelease::parse).parse(input)
        }

        fn implicit_post_release(input: &str) -> IResult<&str, u32> {
            let (input, _) = char('-')(input)?;
            let (input, num) = map_res(digit1, |s: &str| s.parse::<u32>()).parse(input)?;
            Ok((input, num))
        }

        fn explicit_post_release(input: &str) -> IResult<&str, u32> {
            let (input, _) = opt(separator).parse(input)?;
            let (input, _) = alt((tag("post"), tag("r"), tag("rev"))).parse(input)?;
            let (input, num) = opt(digit1).parse(input)?;

            Ok((input, num.unwrap_or("0").parse().unwrap_or(0)))
        }

        fn dev_release(input: &str) -> IResult<&str, u32> {
            let (input, _) = opt(separator).parse(input)?;
            let (input, _) = tag("dev").parse(input)?;
            let (input, num) = opt(digit1).parse(input)?;

            Ok((input, num.unwrap_or("0").parse().unwrap_or(0)))
        }

        fn version_parser(input: &str) -> IResult<&str, PipVersion> {
            let (input, _) = v_prefix(input)?;
            let (input, epoch_opt) = opt(epoch).parse(input)?;
            let (input, release_segs) = release_segments(input)?;
            let (input, pre_rel) = opt(pre_release).parse(input)?;
            let (input, post_rel) =
                opt(alt((explicit_post_release, implicit_post_release))).parse(input)?;
            let (input, dev_rel) = opt(dev_release).parse(input)?;

            Ok((
                input,
                PipVersion {
                    epoch: epoch_opt.unwrap_or(0),
                    release_segments: release_segs,
                    pre_release: pre_rel,
                    post_release: post_rel,
                    dev_release: dev_rel,
                },
            ))
        }

        let input = version.trim();
        match version_parser(input) {
            Ok((remaining, version)) => {
                if !remaining.is_empty() {
                    Err(format!("Unexpected trailing text: '{}'", remaining))
                } else {
                    Ok(version)
                }
            }
            Err(e) => Err(format!("Failed to parse version: {}", e)),
        }
    }
}

impl PartialOrd for PipVersion {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PipVersion {
    fn cmp(&self, other: &Self) -> Ordering {
        let epoch_cmp = self.epoch.cmp(&other.epoch);
        if epoch_cmp != Ordering::Equal {
            return epoch_cmp;
        }

        let max_segments = self
            .release_segments
            .len()
            .max(other.release_segments.len());
        for i in 0..max_segments {
            let self_segment = self.release_segments.get(i).copied().unwrap_or(0);
            let other_segment = other.release_segments.get(i).copied().unwrap_or(0);
            let cmp = self_segment.cmp(&other_segment);
            if cmp != Ordering::Equal {
                return cmp;
            }
        }

        match (self.dev_release, other.dev_release) {
            (None, Some(_)) => return Ordering::Greater,
            (Some(_), None) => return Ordering::Less,
            (Some(self_dev), Some(other_dev)) => {
                let cmp = self_dev.cmp(&other_dev);
                if cmp != Ordering::Equal {
                    return cmp;
                }
            }
            (None, None) => {}
        }

        match (&self.pre_release, &other.pre_release) {
            (None, Some(_)) => return Ordering::Greater,
            (Some(_), None) => return Ordering::Less,
            (Some(self_pre), Some(other_pre)) => {
                let cmp = self_pre.cmp(other_pre);
                if cmp != Ordering::Equal {
                    return cmp;
                }
            }
            (None, None) => {}
        }

        match (self.post_release, other.post_release) {
            (None, Some(_)) => return Ordering::Less,
            (Some(_), None) => return Ordering::Greater,
            (Some(self_post), Some(other_post)) => {
                let cmp = self_post.cmp(&other_post);
                if cmp != Ordering::Equal {
                    return cmp;
                }
            }
            (None, None) => {}
        }
        Ordering::Equal
    }
}

#[cfg(test)]
mod tests {
    use simple_test_case::test_case;

    use super::*;
    use crate::{Revision, constraint};

    const FETCHER: Fetcher = Fetcher::Pip;

    #[test_case("1!2.3.4.a5", PipVersion { epoch: 1, release_segments: vec![2, 3, 4], pre_release: Some(PreRelease::Alpha(5)), post_release: None, dev_release: None }; "epoch1_alpha_short")]
    #[test_case("1!2.3.4.alpha5", PipVersion { epoch: 1, release_segments: vec![2, 3, 4], pre_release: Some(PreRelease::Alpha(5)), post_release: None, dev_release: None }; "epoch1_alpha_no_sep")]
    #[test_case("1!2.3.4-alpha5", PipVersion { epoch: 1, release_segments: vec![2, 3, 4], pre_release: Some(PreRelease::Alpha(5)), post_release: None, dev_release: None }; "epoch1_alpha_dash")]
    #[test_case("1!2.3.4-alpha", PipVersion { epoch: 1, release_segments: vec![2, 3, 4], pre_release: Some(PreRelease::Alpha(0)), post_release: None, dev_release: None }; "epoch1_alpha")]
    #[test_case("1!2.3.4.post5", PipVersion { epoch: 1, release_segments: vec![2, 3, 4], pre_release: None, post_release: Some(5), dev_release: None }; "epoch1_post")]
    #[test_case("1!2.3.4-post5", PipVersion { epoch: 1, release_segments: vec![2, 3, 4], pre_release: None, post_release: Some(5), dev_release: None }; "epoch1_post_hyphen")]
    #[test_case("1!2.3.4_post5", PipVersion { epoch: 1, release_segments: vec![2, 3, 4], pre_release: None, post_release: Some(5), dev_release: None }; "epoch1_post_underscore")]
    #[test_case("1.2.3", PipVersion { epoch: 0, release_segments: vec![1, 2, 3], pre_release: None, post_release: None, dev_release: None }; "simple_version")]
    #[test_case("1!2.3.4_rc5", PipVersion { epoch: 1, release_segments: vec![2, 3, 4], pre_release: Some(PreRelease::RC(5)), post_release: None, dev_release: None }; "epoch1_prerelease")]
    #[test_case("1.2.3_pre4", PipVersion { epoch: 0, release_segments: vec![1, 2, 3], pre_release: Some(PreRelease::RC(4)), post_release: None, dev_release: None }; "prerelease")]
    #[test_case("1.2.3_a", PipVersion { epoch: 0, release_segments: vec![1, 2, 3], pre_release: Some(PreRelease::Alpha(0)), post_release: None, dev_release: None }; "implicit_prerelease")]
    #[test_case("1.2.3-1", PipVersion { epoch: 0, release_segments: vec![1, 2, 3], pre_release: None, post_release: Some(1), dev_release: None }; "implicit_postrelease")]
    #[test]
    fn test_pip_version_parsing(input: &str, expected: PipVersion) {
        let actual = PipVersion::parse(input).expect("should parse version");
        dbg!(&expected, &actual);
        assert_eq!(expected, actual, "compare {expected:?} with {actual:?}");
    }

    #[test_case("== 1.0.0", vec![constraint!(Equal => "1.0.0")]; "1.0.0_eq_1.0.0")]
    #[test_case("~= 2.5", vec![constraint!(Compatible => "2.5")]; "2.5_compat_2.5")]
    #[test_case(">= 1.0, < 2.0", vec![constraint!(GreaterOrEqual => "1.0"), constraint!(Less => "2.0")]; "1.0_geq_1.0_AND_lt_2.0")]
    #[test_case("!= 1.9.3", vec![constraint!(NotEqual => "1.9.3")]; "1.9.3_neq_1.9.3")]
    #[test_case("> 1.0.0a1", vec![constraint!(Greater => "1.0.0a1")]; "1.0.0a1_gt_1.0.0a1")]
    #[test_case("<= 2.0.0.post1", vec![constraint!(LessOrEqual => "2.0.0.post1")]; "2.0.0.post1_leq_2.0.0.post1")]
    #[test_case("== 1.0.0.dev1", vec![constraint!(Equal => "1.0.0.dev1")]; "1.0.0.dev1_eq_1.0.0.dev1")]
    #[test_case("=== 3.0.0", vec![constraint!(Equal => "3.0.0")]; "3.0.0_exact_eq_3.0.0")]
    #[test_case(">= 1!2.0", vec![constraint!(GreaterOrEqual => "1!2.0")]; "1!2.0_geq_1!2.0")]
    #[test]
    fn test_pip_constraints_parsing(input: &str, expected: Vec<Constraint>) {
        let actual = parse_constraints(input).expect("should parse constraint");
        assert_eq!(expected, actual, "compare {expected:?} with {actual:?}");
    }

    #[test_case("$%!@#"; "invalid_special_chars")]
    #[test_case("1.2.3 !!"; "trailing_invalid_chars")]
    #[test_case(">>= 1.0"; "invalid_operator")]
    #[test_case("~= "; "missing_version_after_operator")]
    #[test_case(">= 1.0,"; "trailing_comma")]
    #[test]
    fn test_pip_constraints_parsing_failure(input: &str) {
        parse_constraints(input).expect_err("should not parse constraint");
    }

    #[test_case(constraint!(Equal => "1.0.0"), Revision::from("1.0.0"), true; "equal_versions")]
    #[test_case(constraint!(Equal => "1.0.0"), Revision::from("1.0.0.post1"), false; "post_release_not_equal")]
    #[test_case(constraint!(GreaterOrEqual => "1.0.0"), Revision::from("1.0.0"), true; "greater_equal_same")]
    #[test_case(constraint!(GreaterOrEqual => "1.0.0"), Revision::from("0.9.0"), false; "not_greater_equal")]
    #[test_case(constraint!(Less => "2.0.0"), Revision::from("1.9.9"), true; "less_than")]
    #[test_case(constraint!(Less => "1.0.0"), Revision::from("1.0.0"), false; "not_less_equal")]
    #[test_case(constraint!(Compatible => "1.2"), Revision::from("1.2.5"), true; "1.2_compatible_version_1.3.5")]
    #[test_case(constraint!(Compatible => "1.2.3"), Revision::from("1.2.5"), true; "compatible_version")]
    #[test_case(constraint!(Compatible => "1.2.3"), Revision::from("1.3.0"), false; "not_compatible_version")]
    #[test_case(constraint!(Equal => "1.0.0a1"), Revision::from("1.0.0a1"), true; "prerelease_equal")]
    #[test_case(constraint!(Greater => "1.0.0a1"), Revision::from("1.0.0"), true; "final_greater_than_prerelease")]
    #[test_case(constraint!(Greater => "1.0.0"), Revision::from("1.0.0.post1"), true; "post_greater_than_final")]
    #[test_case(constraint!(Less => "1.0.0"), Revision::from("1.0.0.dev1"), true; "dev_less_than_final")]
    #[test_case(constraint!(Equal => "1!1.0.0"), Revision::from("1!1.0.0"), true; "equal_with_epoch")]
    #[test_case(constraint!(Greater => "0!2.0.0"), Revision::from("1!1.0.0"), true; "greater_epoch")]
    #[test]
    fn test_pip_version_comparison(constraint: Constraint, target: Revision, expected: bool) {
        assert_eq!(
            compare(&constraint, FETCHER, &target).expect("should not have a parse error"),
            expected,
            "compare '{target}' to '{constraint}', expected: {expected}"
        );
    }

    #[test_case("1.0.0", "1.0.0", Ordering::Equal; "equal_normal_versions")]
    #[test_case("1.1.0", "1.0.0", Ordering::Greater; "greater_minor_version")]
    #[test_case("1.0.0", "1.1.0", Ordering::Less; "less_minor_version")]
    #[test_case("1.0.0", "1.0.0a1", Ordering::Greater; "final_greater_than_prerelease")]
    #[test_case("1.0.0a1", "1.0.0b1", Ordering::Less; "alpha_less_than_beta")]
    #[test_case("1.0.0b1", "1.0.0rc1", Ordering::Less; "beta_less_than_rc")]
    #[test_case("1.0.0.post1", "1.0.0", Ordering::Greater; "post_greater_than_final")]
    #[test_case("1.0.0.dev1", "1.0.0", Ordering::Less; "dev_less_than_final")]
    #[test_case("1.0.0.dev1", "1.0.0a1", Ordering::Less; "dev_less_than_prerelease")]
    #[test_case("1!1.0.0", "2.0.0", Ordering::Greater; "epoch_takes_precedence")]
    #[test]
    fn test_pip_version_ordering(version1: &str, version2: &str, expected: Ordering) {
        let v1 = PipVersion::parse(version1).expect("valid version");
        let v2 = PipVersion::parse(version2).expect("valid version");
        assert_eq!(
            v1.cmp(&v2),
            expected,
            "Expected {version1} to be {expected:?} {version2}"
        );
    }
}
