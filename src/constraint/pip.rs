//! # Python Package Index (PyPI) Requirements and Constraints
//!
//! Implements Python's version specification system (PEP 440) for parsing, comparing,
//! and evaluating version constraints. This module enables accurate constraint checking
//! for Python packages based on PyPI's specific versioning rules.
//!
//! ## Key Features
//!
//! - **Complete PEP 440 support**: Implements Python's standardized version specification
//! - **Multi-component versioning**: Handles Python's complex version format with epochs, pre/post/dev releases
//! - **Normalization**: Correctly handles Python's version normalization rules
//! - **Constraint operators**: Supports all standard Python version specifiers (`==`, `!=`, `>=`, `<=`, `>`, `<`, `~=`)
//!
//! ## Requirement Format
//!
//! Python versions follow this structure: `[N!]N(.N)*[{a|b|rc|...}N][.postN][.devN]`
//!
//! For example: `1!2.3.4a5.post6.dev7`
//!
//! This structure consists of five distinct components:
//!
//! - **Epoch**: `N!` - Overrides normal version ordering (defaults to `0!`)
//! - **Release**: `N(.N)*` - Core version numbers (like `1.2.3`)
//! - **Pre-release**: Various formats like `a1` (alpha), `b2` (beta), `rc3`
//! - **Post-release**: `.post1` - Indicates post-release fixes
//! - **Development**: `.dev1` - Indicates development releases
//!
//! ## Ordering Rules
//!
//! Python's version ordering follows specific rules:
//!
//! 1. Epoch segments are compared first (higher is newer)
//! 2. Release segments are compared numerically from left to right
//! 3. Development releases (`dev`) are older than pre-releases
//! 4. Pre-releases (`a` < `b` < `rc`) are older than final releases
//! 5. Final releases are older than post releases (`post`)
//!
//! ## Normalization Rules
//!
//! PEP 440 specifies normalization for different version formats:
//!
//! - Case-insensitive comparison of all segments
//! - Numeric segments with leading zeros are normalized (e.g., `01.2` -> `1.2`)
//! - Multiple pre-release separators (`.`, `-`, `_`) all normalize to a single format
//! - Various pre-release labels map to standard forms (e.g., `alpha` -> `a`, `preview` -> `rc`)
//! - Post-release variants (`r`, `rev`) normalize to `post`
//!
//! ## Integration
//!
//! This module integrates with the crate's [`Comparable`](super::Comparable) trait system,
//! enabling Python versions to work with the generic [`Constraint`](super::Constraint)
//! and [`Constraints`](super::Constraints) types.
//!
//! ## References
//!
//! - [PEP 440 - Requirement Identification](https://peps.python.org/pep-0440/)
//! - [Requirement Specifiers](https://packaging.python.org/en/latest/specifications/version-specifiers/)
//! - [Requirement Specifiers](https://pip.pypa.io/en/stable/reference/requirement-specifiers/)
//!
use std::cmp::Ordering;

use bon::Builder;
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, multispace0, u32},
    combinator::{eof, map_res, opt, value},
    multi::{many1, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
};
use thiserror::Error;
use tracing::warn;

use super::{Comparable, Constraint, Constraints};
use crate::Revision;

/// A version requirement in pip.
///
/// This structure represents Python package version requirements, following the PEP 440
/// specification for version identification and comparison.
///
/// ## Requirement Format
///
/// Python version requirements follow this structure: `[N!]N(.N)*[{a|b|rc|...}N][.postN][.devN]`
///
/// For example: `1!2.3.4a5.post6.dev7`
///
/// ## Comparison Rules
///
/// Python's versioning system has precise rules for:
/// - Component precedence (epoch > release > pre-release > post-release > dev)
/// - Pre-release designations (alpha < beta < release candidate)
/// - Implicit zeros (when components are omitted)
///
/// See [PEP 440](https://peps.python.org/pep-0440/) for more information.
#[derive(Debug, Clone, PartialEq, Eq, Builder)]
struct Requirement {
    /// The epoch of the version; most versions have an epoch of 0, which is the default.
    /// After, for example, changing from a `yyyy.mm.dd` version format to a semver format, a project might increment its epoch to capture the change.
    /// That would allow the project to indicate that every semver version was later than any date-marked version.
    /// In a version `0!1.2.3.rc4.post5.dev6`, the epoch is 0.
    #[builder(default = 0)]
    epoch: u32,

    /// The release segments of the version.
    /// These are what we most commonly picture when we imagine a version.
    /// For example, in the version `1.2.3`, the release segments are `[1, 2, 3]`.
    /// In a version `0!1.2.3.rc4.post5.dev6`, the release segments are `[1, 2, 3]`.
    #[builder(default, into)]
    segments: Vec<u32>,

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

/// Errors encountered while parsing pypi constraints.
#[derive(Error, Clone, PartialEq, Eq, Debug)]
pub enum Error {
    /// Parsing constraints.
    #[error("parse constraints {constraints:?}: {message:?})")]
    ParseConstraint {
        /// The constraints being parsed.
        constraints: String,

        /// The error message.
        message: String,
    },

    /// Parsing versions.
    #[error("parse version {version:?}: {message:?})")]
    ParseRequirement {
        /// The version being parsed.
        version: String,

        /// The error message.
        message: String,
    },
}

impl Comparable<Requirement> for Revision {
    fn compatible(&self, target: &Requirement) -> bool {
        let Ok(target) = Requirement::try_from(self) else {
            return self.compatible(&Revision::from(target));
        };

        let threshold_segments = target.segments.clone();
        if threshold_segments.len() >= 2 {
            let min_version = target.clone();

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
            let max_version = Requirement::builder()
                .epoch(target.epoch)
                .segments(max_segments)
                .build();

            target >= min_version && target < max_version
        } else {
            warn!("Not enough release segments for compatible operator with {target}");
            false
        }
    }
    fn equal(&self, v: &Requirement) -> bool {
        match Requirement::try_from(self) {
            Ok(ref source) => source == v,
            Err(_) => self.equal(&Revision::from(v)),
        }
    }

    fn less(&self, v: &Requirement) -> bool {
        match Requirement::try_from(self) {
            Ok(ref source) => source < v,
            Err(_) => self.less(&Revision::from(v)),
        }
    }

    fn greater(&self, v: &Requirement) -> bool {
        match Requirement::try_from(self) {
            Ok(ref source) => source > v,
            Err(_) => self.greater(&Revision::from(v)),
        }
    }

    fn not_equal(&self, v: &Requirement) -> bool {
        match Requirement::try_from(self) {
            Ok(ref source) => source != v,
            Err(_) => self.not_equal(&Revision::from(v)),
        }
    }

    fn less_or_equal(&self, v: &Requirement) -> bool {
        match Requirement::try_from(self) {
            Ok(ref source) => source <= v,
            Err(_) => self.less_or_equal(&Revision::from(v)),
        }
    }

    fn greater_or_equal(&self, v: &Requirement) -> bool {
        match Requirement::try_from(self) {
            Ok(ref source) => source >= v,
            Err(_) => self.greater_or_equal(&Revision::from(v)),
        }
    }
}

// Important: In constraint.rs, what matters is the relationship:
// constraint.matches(&target) = Does the TARGET match the CONSTRAINT?
// For example, with "< 2.0.0" constraint and target "1.9.9":
// Is "1.9.9" < "2.0.0"? Yes, so it matches.
//
// This means for the Requirement.less(Revision) we're checking:
// Is the target revision LESS THAN this constraint version?
impl Comparable<Revision> for Requirement {
    fn compatible(&self, v: &Revision) -> bool {
        let Ok(target_version) = Requirement::try_from(v) else {
            return Revision::from(self).compatible(v);
        };

        let threshold_segments = self.segments.clone();
        if threshold_segments.len() >= 2 {
            let min_version = self.clone();

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
            let max_version = Requirement::builder()
                .epoch(self.epoch)
                .segments(max_segments)
                .build();

            target_version >= min_version && target_version < max_version
        } else {
            warn!("Not enough release segments for compatible operator with {self}");
            false
        }
    }

    fn equal(&self, v: &Revision) -> bool {
        match Requirement::try_from(v) {
            Ok(ref target_version) => self == target_version,
            Err(_) => Revision::from(self).equal(v),
        }
    }

    // For Less constraint (< 2.0.0), we check if target < constraint_version
    // Is the target revision LESS THAN this constraint version?
    fn less(&self, v: &Revision) -> bool {
        match Requirement::try_from(v) {
            Ok(ref target_version) => target_version < self,
            Err(_) => Revision::from(self).less(v),
        }
    }

    // For Greater constraint (> 1.0.0), we check if target > constraint_version
    // Is the target revision GREATER THAN this constraint version?
    fn greater(&self, v: &Revision) -> bool {
        match Requirement::try_from(v) {
            Ok(ref target_version) => target_version > self,
            Err(_) => Revision::from(self).greater(v),
        }
    }

    fn not_equal(&self, v: &Revision) -> bool {
        match Requirement::try_from(v) {
            Ok(ref target_version) => self != target_version,
            Err(_) => Revision::from(self).not_equal(v),
        }
    }

    fn less_or_equal(&self, v: &Revision) -> bool {
        match Requirement::try_from(v) {
            Ok(ref target_version) => target_version <= self,
            Err(_) => Revision::from(self).less_or_equal(v),
        }
    }

    fn greater_or_equal(&self, v: &Revision) -> bool {
        match Requirement::try_from(v) {
            Ok(ref target_version) => target_version >= self,
            Err(_) => Revision::from(self).greater_or_equal(v),
        }
    }
}

/// Parse a pypi requirements string into [`Constraints`].
#[tracing::instrument]
pub fn parse(input: &str) -> Result<Constraints<Requirement>, Error> {
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

    fn single_constraint(input: &str) -> IResult<&str, Constraint<Requirement>> {
        let (input, (op, rev)) = pair(
            delimited(multispace0, operator, multispace0),
            delimited(multispace0, Requirement::parser, multispace0),
        )
        .parse(input)?;

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

impl std::fmt::Display for Requirement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}!", self.epoch)?;
        let mut is_first = true;
        for segment in &self.segments {
            if is_first {
                is_first = false;
                write!(f, "{segment}")?;
            } else {
                write!(f, ".{segment}")?;
            }
        }
        if let Some(pre_release) = &self.pre_release {
            write!(f, "{pre_release}")?;
        }
        if let Some(post_release) = &self.post_release {
            write!(f, ".post{post_release}")?;
        }
        if let Some(dev_release) = &self.dev_release {
            write!(f, ".dev{dev_release}")?;
        }
        Ok(())
    }
}

impl From<Requirement> for Revision {
    fn from(v: Requirement) -> Self {
        Self::Opaque(v.to_string())
    }
}

impl From<&Requirement> for Revision {
    fn from(v: &Requirement) -> Self {
        Self::Opaque(v.to_string())
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
    Rc(u32),
}

impl std::fmt::Display for PreRelease {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PreRelease::Alpha(n) => write!(f, "a{n}"),
            PreRelease::Beta(n) => write!(f, "b{n}"),
            PreRelease::Rc(n) => write!(f, "rc{n}"),
        }
    }
}

impl PreRelease {
    fn number(&self) -> u32 {
        match self {
            PreRelease::Alpha(n) => *n,
            PreRelease::Beta(n) => *n,
            PreRelease::Rc(n) => *n,
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
            Ok((input, PreRelease::Rc(number.unwrap_or(0))))
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
            (PreRelease::Alpha(_), PreRelease::Rc(_)) => Ordering::Less,
            (PreRelease::Beta(_), PreRelease::Alpha(_)) => Ordering::Greater,
            (PreRelease::Beta(_), PreRelease::Rc(_)) => Ordering::Less,
            (PreRelease::Rc(_), PreRelease::Alpha(_)) => Ordering::Greater,
            (PreRelease::Rc(_), PreRelease::Beta(_)) => Ordering::Greater,
            (lhs, rhs) => lhs.number().cmp(&rhs.number()),
        }
    }
}

impl TryFrom<&Revision> for Requirement {
    type Error = Error;

    fn try_from(rev: &Revision) -> Result<Self, Self::Error> {
        match rev {
            Revision::Semver(semver) => {
                let pre_release = if semver.pre.is_empty() {
                    None
                } else {
                    PreRelease::parse(semver.pre.as_str())
                        .map(|(_, pre)| Some(pre))
                        .map_err(|e| Error::ParseRequirement {
                            version: semver.to_string(),
                            message: format!("invalid pre-release version: {e:?}"),
                        })?
                };
                let release_segments = vec![
                    semver.major as u32,
                    semver.minor as u32,
                    semver.patch as u32,
                ];
                let builder = Requirement::builder().segments(release_segments);

                let mut version = if let Some(pre_release) = pre_release {
                    builder.pre_release(pre_release).build()
                } else {
                    builder.build()
                };

                let pre_opt = &semver.pre;
                if !pre_opt.is_empty() {
                    if let Ok((_, pre_release)) = PreRelease::parse(pre_opt.as_str()) {
                        version.pre_release = Some(pre_release);
                    } else {
                        return Err(Error::ParseRequirement {
                            version: semver.to_string(),
                            message: format!("Unexpected pre-release: {}", pre_opt),
                        });
                    }
                }

                Ok(version)
            }
            Revision::Opaque(opaque) => {
                Requirement::parse(opaque).map_err(|e| Error::ParseRequirement {
                    version: opaque.to_string(),
                    message: e.to_string(),
                })
            }
        }
    }
}

impl Requirement {
    /// Parses and normalizes a pip version
    fn parse(version: &str) -> Result<Self, String> {
        let input = version.trim();
        match Requirement::parser(input) {
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

    fn parser(input: &str) -> IResult<&str, Requirement> {
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

        let (input, _) = v_prefix(input)?;
        let (input, epoch_opt) = opt(epoch).parse(input)?;
        let (input, release_segs) = release_segments(input)?;
        let (input, pre_rel) = opt(pre_release).parse(input)?;
        let (input, post_rel) =
            opt(alt((explicit_post_release, implicit_post_release))).parse(input)?;
        let (input, dev_rel) = opt(dev_release).parse(input)?;

        Ok((
            input,
            Requirement {
                epoch: epoch_opt.unwrap_or(0),
                segments: release_segs,
                pre_release: pre_rel,
                post_release: post_rel,
                dev_release: dev_rel,
            },
        ))
    }
}

impl PartialOrd for Requirement {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Requirement {
    fn cmp(&self, other: &Self) -> Ordering {
        let epoch_cmp = self.epoch.cmp(&other.epoch);
        if epoch_cmp != Ordering::Equal {
            return epoch_cmp;
        }

        let max_segments = self.segments.len().max(other.segments.len());
        for i in 0..max_segments {
            let self_segment = self.segments.get(i).copied().unwrap_or(0);
            let other_segment = other.segments.get(i).copied().unwrap_or(0);
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
    use crate::{Revision, constraint, constraints};

    macro_rules! version {
        ($($field:ident = $value:expr),*) => {
            Requirement::builder()
                $(.$field($value))*
                .build()
        };
    }

    macro_rules! pre {
        ($kind:ident, $value:expr) => {
            PreRelease::$kind($value)
        };
    }

    #[test_case("1!2.3.4.a5", version!(epoch = 1, segments = [2, 3, 4], pre_release = pre!(Alpha, 5)); "epoch1_alpha_short")]
    #[test_case("1!2.3.4.alpha5", version!(epoch = 1, segments = [2, 3, 4], pre_release = pre!(Alpha, 5)); "epoch1_alpha_no_sep")]
    #[test_case("1!2.3.4-alpha5", version!(epoch = 1, segments = [2, 3, 4], pre_release = pre!(Alpha, 5)); "epoch1_alpha_dash")]
    #[test_case("1!2.3.4-alpha", version!(epoch = 1, segments = [2, 3, 4], pre_release = pre!(Alpha, 0)); "epoch1_alpha")]
    #[test_case("1!2.3.4.post5", version!(epoch = 1, segments = [2, 3, 4], post_release = 5); "epoch1_post")]
    #[test_case("1!2.3.4-post5", version!(epoch = 1, segments = [2, 3, 4], post_release = 5); "epoch1_post_hyphen")]
    #[test_case("1!2.3.4_post5", version!(epoch = 1, segments = [2, 3, 4], post_release = 5); "epoch1_post_underscore")]
    #[test_case("1.2.3", version!(segments = [1, 2, 3]); "simple_version")]
    #[test_case("1!2.3.4_rc5", version!(epoch = 1, segments = [2, 3, 4], pre_release = pre!(Rc, 5)); "epoch1_prerelease")]
    #[test_case("1.2.3_pre4", version!(segments = [1, 2, 3], pre_release = pre!(Rc, 4)); "prerelease")]
    #[test_case("1.2.3_a", version!(segments = [1, 2, 3], pre_release = pre!(Alpha, 0)); "implicit_prerelease")]
    #[test_case("1.2.3-1", version!(segments = [1, 2, 3], post_release = 1); "implicit_postrelease")]
    #[test]
    fn pip_version_parsing(input: &str, expected: Requirement) {
        let actual = Requirement::parse(input).expect("should parse version");
        assert_eq!(expected, actual, "compare {expected:?} with {actual:?}");
    }

    #[test_case("== 1.0.0", constraints!({ Equal => version!(segments = [1, 0, 0]) }); "1.0.0_eq_1.0.0")]
    #[test_case("~= 2.5", constraints!({ Compatible => version!(segments = [2, 5]) }); "2.5_compat_2.5")]
    #[test_case("!= 1.9.3", constraints!({ NotEqual => version!(segments = [1, 9, 3]) }); "1.9.3_neq_1.9.3")]
    #[test_case("> 1.0.0a1", constraints!({ Greater => version!(segments = [1, 0, 0], pre_release = pre!(Alpha, 1)) }); "1.0.0a1_gt_1.0.0a1")]
    #[test_case("<= 2.0.0.post1", constraints!({ LessOrEqual => version!(segments = [2, 0, 0], post_release = 1) }); "2.0.0.post1_leq_2.0.0.post1")]
    #[test_case("== 1.0.0.dev1", constraints!({ Equal => version!(segments = [1, 0, 0], dev_release = 1) }); "1.0.0.dev1_eq_1.0.0.dev1")]
    #[test_case("=== 3.0.0", constraints!({ Equal => version!(segments = [3, 0, 0]) }); "3.0.0_exact_eq_3.0.0")]
    #[test_case(">= 1!2.0", constraints!({ GreaterOrEqual => version!(epoch = 1, segments = [2, 0]) }); "1!2.0_geq_1!2.0")]
    #[test_case(
        ">= 1.0, < 2.0",
        constraints!(
            { GreaterOrEqual => version!(segments = [1, 0]) },
            { Less => version!(segments = [2, 0]) },
        );
        "1.0_geq_1.0_AND_lt_2.0"
    )]
    #[test]
    fn pip_constraints_parsing(input: &str, expected: Constraints<Requirement>) {
        let actual = parse(input).expect("should parse constraint");
        assert_eq!(expected, actual, "compare {expected:?} with {actual:?}");
    }

    #[test_case("$%!@#"; "invalid_special_chars")]
    #[test_case("1.2.3 !!"; "trailing_invalid_chars")]
    #[test_case(">>= 1.0"; "invalid_operator")]
    #[test_case("~= "; "missing_version_after_operator")]
    #[test_case(">= 1.0,"; "trailing_comma")]
    #[test]
    fn pip_constraints_parsing_failure(input: &str) {
        parse(input).expect_err("should not parse constraint");
    }

    #[test_case(constraint!(Equal => version!(segments = [1, 0, 0])), Revision::from("1.0.0"), true; "equal_versions")]
    #[test_case(constraint!(Equal => version!(segments = [1, 0, 0])), Revision::from("1.0.0.post1"), false; "post_release_not_equal")]
    #[test_case(constraint!(Equal => version!(segments = [1, 0, 0], pre_release = pre!(Alpha, 1))), Revision::from("1.0.0a1"), true; "prerelease_equal")]
    #[test_case(constraint!(Equal => version!(epoch = 1, segments = [1, 0, 0])), Revision::from("1!1.0.0"), true; "equal_with_epoch")]
    #[test_case(constraint!(GreaterOrEqual => version!(segments = [1, 0, 0])), Revision::from("1.0.0"), true; "greater_equal_same")]
    #[test_case(constraint!(GreaterOrEqual => version!(segments = [1, 0, 0])), Revision::from("0.9.0"), false; "not_greater_equal")]
    #[test_case(constraint!(Less => version!(segments = [2, 0, 0])), Revision::from("1.9.9"), true; "less_than")]
    #[test_case(constraint!(Less => version!(segments = [1, 0, 0])), Revision::from("1.0.0"), false; "not_less_than")]
    #[test_case(constraint!(Less => version!(segments = [1, 0, 0])), Revision::from("1.0.0.dev1"), true; "dev_less_than_final")]
    #[test_case(constraint!(Compatible => version!(segments = [1, 2])), Revision::from("1.2.5"), true; "1.2_compatible_version_1.2.5")]
    #[test_case(constraint!(Compatible => version!(segments = [1, 2, 3])), Revision::from("1.2.5"), true; "compatible_version")]
    #[test_case(constraint!(Compatible => version!(segments = [1, 2, 3])), Revision::from("1.3.0"), false; "not_compatible_version")]
    #[test_case(constraint!(Greater => version!(segments = [1, 0, 0], pre_release = pre!(Alpha, 1))), Revision::from("1.0.0"), true; "final_greater_than_prerelease")]
    #[test_case(constraint!(Greater => version!(segments = [1, 0, 0])), Revision::from("1.0.0.post1"), true; "post_greater_than_final")]
    #[test_case(constraint!(Less => version!(epoch = 1, segments = [1, 0, 0])), Revision::from("1.1.0"), true; "lower_epoch_less_than_higher")]
    #[test]
    fn pip_version_comparison(
        constraint: Constraint<Requirement>,
        target: Revision,
        expected: bool,
    ) {
        assert_eq!(
            constraint.matches(&target),
            expected,
            "check if version '{target}' matches constraint '{constraint}', expected: {expected}"
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
    fn pip_version_ordering(version1: &str, version2: &str, expected: Ordering) {
        let v1 = Requirement::parse(version1).expect("valid version");
        let v2 = Requirement::parse(version2).expect("valid version");
        assert_eq!(
            v1.cmp(&v2),
            expected,
            "Expected {version1} to be {expected:?} {version2}"
        );
    }
}
