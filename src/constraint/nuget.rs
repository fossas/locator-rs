//! # NuGet Versions and Constraints
//!
//! This module implements package version parsing and comparison for NuGet versions,
//! and constraint checking using ranges from the GHSA API.
//!
//! ## Version Format
//!
//! > NuGet considers a package version to be SemVer v2.0.0 specific if either of the following statements is true:
//! > - The pre-release label is dot-separated, for example, 1.0.0-alpha.1
//! > - The version has build-metadata, for example, 1.0.0+githash
//!
//! NuGet versions follow SemVer 1.0.0 or 2.0.0 with these key differences:
//! - Optional 4th segment (Revision): `Major.Minor.Patch.Revision`
//! - Build metadata ignored in comparisons
//! - Only major version required (others default to 0)
//! - Case-insensitive prerelease comparisons
//!
//! ## References
//!
//! - [NuGet SemVer 1/2 rules](https://learn.microsoft.com/en-us/nuget/concepts/package-versioning?tabs=semver20sort#semantic-versioning-200)
//! - [NuGet Versioning](https://learn.microsoft.com/en-us/nuget/concepts/package-versioning)
//! - [Version Normalization](https://learn.microsoft.com/en-us/nuget/concepts/package-versioning?tabs=semver20sort#normalized-version-numbers)
//! - [GHSA API](https://docs.github.com/en/rest/security-advisories/global-advisories)
//! - [OSV ranges](https://ossf.github.io/osv-schema/#affectedranges-field)

use std::{cmp::Ordering, fmt::Write, str::FromStr};

use bon::Builder;
use derivative::Derivative;
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, multispace0},
    combinator::eof,
    multi::separated_list1,
    sequence::{delimited, pair, terminated},
};
use thiserror::Error;

use super::{Comparable, Constraint};
use crate::Revision;

// Implementation of the Comparable trait for NuGet's Version type
// This enables constraint checking against different version types

impl Comparable<Revision> for Version {
    /// Compatibility check for NuGet versions, following NuGet's convention.
    /// For NuGet, the compatible operator (~=) works as follows:
    /// - If any of minor, patch, or revision is > 0, increment the leftmost non-zero component
    ///   and set all components to the right to zero.
    /// - Target version must be between [min_version, max_version)
    fn compatible(&self, rev: &Revision) -> bool {
        if let Ok(target) = Version::try_from(rev.to_string()) {
            // Configure the version range [min, max) for compatibility
            let min_version = self.clone();
            let mut max_version = min_version.clone();
            
            if min_version.minor > 0 || min_version.patch > 0 || min_version.revision > 0 {
                if min_version.revision > 0 {
                    // 1.2.3.4 is compatible with [1.2.3.4, 1.2.4.0)
                    max_version.patch += 1;
                    max_version.revision = 0;
                } else if min_version.patch > 0 {
                    // 1.2.3 is compatible with [1.2.3, 1.3.0.0)
                    max_version.minor += 1;
                    max_version.patch = 0;
                    max_version.revision = 0;
                } else {
                    // 1.2.0 is compatible with [1.2.0, 2.0.0.0)
                    max_version.major += 1;
                    max_version.minor = 0;
                    max_version.patch = 0;
                    max_version.revision = 0;
                }
            } else {
                // 1.0.0 is compatible with [1.0.0, 2.0.0.0)
                max_version.major += 1;
                max_version.minor = 0;
                max_version.patch = 0;
                max_version.revision = 0;
            }
            
            target >= min_version && target < max_version
        } else {
            // If we can't parse the target as a NuGet version,
            // fallback to string comparison (this is rare but possible)
            self.to_string() == rev.to_string()
        }
    }

    /// Equality check for NuGet versions
    fn equal(&self, rev: &Revision) -> bool {
        if let Ok(target) = Version::try_from(rev.to_string()) {
            // NuGet versions are equal if they're the same after normalization
            // (e.g., 1.0 == 1.0.0 == 1.0.0.0)
            self == &target
        } else {
            false
        }
    }

    /// Less than check for NuGet versions
    fn less(&self, rev: &Revision) -> bool {
        if let Ok(target) = Version::try_from(rev.to_string()) {
            // For a constraint like "< 2.0.0", the target version must be less than the constraint version
            // So if constraint is "< 2.0.0" and target is "1.9.9", then 1.9.9 < 2.0.0, which is true
            
            // Compare by cloning to avoid reference issues
            let self_clone = self.clone();
            target < self_clone
        } else {
            false
        }
    }

    /// Greater than check for NuGet versions
    fn greater(&self, rev: &Revision) -> bool {
        if let Ok(target) = Version::try_from(rev.to_string()) {
            // For a constraint like "> 1.0.0", the target version must be greater than the constraint version
            // So if constraint is "> 1.0.0" and target is "1.1.0", then 1.1.0 > 1.0.0, which is true
            
            // Compare by cloning to avoid reference issues
            let self_clone = self.clone();
            target > self_clone
        } else {
            false
        }
    }

    /// Not equal check for NuGet versions
    fn not_equal(&self, rev: &Revision) -> bool {
        !self.equal(rev)
    }

    /// Less than or equal check for NuGet versions
    fn less_or_equal(&self, rev: &Revision) -> bool {
        if let Ok(target) = Version::try_from(rev.to_string()) {
            // For a constraint like "<= 2.0.0", the target version must be less than or equal to the constraint version
            
            // Compare by cloning to avoid reference issues
            let self_clone = self.clone();
            target <= self_clone
        } else {
            false
        }
    }

    /// Greater than or equal check for NuGet versions
    fn greater_or_equal(&self, rev: &Revision) -> bool {
        if let Ok(target) = Version::try_from(rev.to_string()) {
            // For a constraint like ">= 1.0.0", the target version must be greater than or equal to the constraint version
            
            // Compare by cloning to avoid reference issues
            let self_clone = self.clone();
            target >= self_clone
        } else {
            false
        }
    }
}

// Implementation of the Comparable trait for NuGet's Version to Version comparison
impl Comparable<Version> for Version {
    fn compatible(&self, other: &Version) -> bool {
        let min_version = self.clone();
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
        } else {
            max_version.major += 1;
            max_version.minor = 0;
            max_version.patch = 0;
            max_version.revision = 0;
        }
        
        other >= &min_version && other < &max_version
    }

    fn equal(&self, other: &Version) -> bool {
        self == other
    }

    fn less(&self, other: &Version) -> bool {
        // Create clones to handle value comparison
        let self_clone = self.clone();
        let other_clone = other.clone();
        other_clone < self_clone
    }

    fn greater(&self, other: &Version) -> bool {
        // Create clones to handle value comparison
        let self_clone = self.clone();
        let other_clone = other.clone();
        other_clone > self_clone
    }

    fn not_equal(&self, other: &Version) -> bool {
        self != other
    }

    fn less_or_equal(&self, other: &Version) -> bool {
        // Create clones to handle value comparison
        let self_clone = self.clone();
        let other_clone = other.clone();
        other_clone <= self_clone
    }

    fn greater_or_equal(&self, other: &Version) -> bool {
        // Create clones to handle value comparison
        let self_clone = self.clone();
        let other_clone = other.clone();
        other_clone >= self_clone
    }
}

// We also need to implement Comparable<Version> for Revision
// to enable bidirectional comparison
impl Comparable<Version> for Revision {
    fn compatible(&self, other: &Version) -> bool {
        match Version::try_from(self.to_string()) {
            Ok(self_version) => self_version.compatible(other),
            Err(_) => false,
        }
    }

    fn equal(&self, other: &Version) -> bool {
        match Version::try_from(self.to_string()) {
            Ok(self_version) => self_version.equal(other),
            Err(_) => false,
        }
    }

    fn not_equal(&self, other: &Version) -> bool {
        !self.equal(other)
    }

    fn less(&self, other: &Version) -> bool {
        match Version::try_from(self.to_string()) {
            Ok(self_version) => self_version.less(other),
            Err(_) => false,
        }
    }

    fn greater(&self, other: &Version) -> bool {
        match Version::try_from(self.to_string()) {
            Ok(self_version) => self_version.greater(other),
            Err(_) => false,
        }
    }

    fn less_or_equal(&self, other: &Version) -> bool {
        match Version::try_from(self.to_string()) {
            Ok(self_version) => self_version.less_or_equal(other),
            Err(_) => false,
        }
    }

    fn greater_or_equal(&self, other: &Version) -> bool {
        match Version::try_from(self.to_string()) {
            Ok(self_version) => self_version.greater_or_equal(other),
            Err(_) => false,
        }
    }
}

// Note:
// Much of this was brought over from `sparkle`.
// At some point after we've rolled this out, we will want to deduplicate this code.

/// Package versions, as implemented by Nuget.
///
/// Nuget package versions are mostly either Semver 1.0.0 or Semver 2.0.0,
/// except that they also optionally support a 4th segment (`Revision`).
///
/// ## Ordering
///
/// Ordering for a collection of [`Version`] orders according to Nuget rules,
/// in order of "oldest release" to "newest release" in versioning order.
///
/// Note that when using the `Ord` implementations,
/// we can only compare two versions, so it's possible for a set that contains
/// a mixture of [`VersionKind::Semver1`] and [`VersionKind::Semver2`] versions
/// may have confusing sorts unless the kinds are normalized
/// (using [`Version::override_kind`]).
///
/// For convenience you can use the [`Versions`] collection,
/// which performs this normalization automatically.
///
/// ## Reference
///
/// - https://learn.microsoft.com/en-us/nuget/concepts/package-versioning
//
// Note that `Version` effectively contains a generic semver2 and semver1 parser and comparator,
// it just has extra semantics from Nuget. When we parse other versions in the future
// we can start thinking about how to abstract this to reduce duplicate logic.
//
// I didn't go ahead and do this now because I didn't want to inflict premature
// abstractions on us; I'm not sure what other specific changes in semantics other
// package managers will require.
//
// Most likely this'll look like "extract shared primitives and use them
// when defining other package-manager-specific version types",
// but it's _possible_ we may be able to model this in traits as some form
// of extensions to the base parsing/comparison rules.
#[derive(Clone, Debug, Builder, Derivative)]
#[derivative(Hash)]
#[non_exhaustive]
pub struct Version {
    /// The major version.
    pub major: usize,

    /// The minor version.
    #[builder(default)]
    pub minor: usize,

    /// The patch version.
    #[builder(default)]
    pub patch: usize,

    /// The revision version.
    #[builder(default)]
    pub revision: usize,

    /// The pre-release label of the overall version.
    #[builder(into)]
    pub label: Option<String>,

    /// The build metadata of the version.
    // According to semver and to nuget, the build metadata is not considered for equality or ordering.
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[builder(into)]
    pub build_meta: Option<String>,

    // Allows overriding the semver determination of the version,
    // in case we _know_ a collection of versions should be compared
    // as semver 2.0.0; this allows us to avoid confusing results
    // where some versions are sorted with the 2.0.0 rules and some aren't.
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    override_kind: Option<VersionKind>,
}

impl Version {
    /// Report whether the version is a pre-release version.
    pub fn is_prerelease(&self) -> bool {
        self.label.is_some()
    }

    /// Render the minimal form of a version.
    ///
    /// In Nuget, versions imply a following implicit ".0" of every field not specified;
    /// as in "1" is equivalent to "1.0.0.0". This method renders the version with any
    /// trailing zero-valued segments omitted: the version "1.0.0.0" is rendered as "1".
    ///
    /// Note that since segments are positionally important, this method still renders
    /// zero-valued segments between significant segments- e.g. the version "1.0.1.0" is
    /// rendered as "1.0.1", which is the miniman form able to reconstruct the original version.
    pub fn to_string_minimal(&self) -> String {
        let mut buf = String::new();
        let major = self.major;

        // If latter parts of the version are non-zero, the preceding parts must be written out,
        // since parsing version segments is order-dependent.
        // Otherwise we can elide the zero segments.
        match (self.minor, self.patch, self.revision) {
            (minor, patch, rev @ 1..) => {
                write!(&mut buf, "{major}.{minor}.{patch}.{rev}").expect("write to buffer")
            }
            (minor, patch @ 1.., 0) => {
                write!(&mut buf, "{major}.{minor}.{patch}").expect("write to buffer")
            }
            (minor @ 1.., 0, 0) => write!(&mut buf, "{major}.{minor}").expect("write to buffer"),
            (0, 0, 0) => write!(&mut buf, "{major}").expect("write to buffer"),
        }
        if let Some(suffix) = &self.label {
            write!(&mut buf, "-{suffix}").expect("write to buffer");
        }
        if let Some(build) = &self.build_meta {
            write!(&mut buf, "+{build}").expect("write to buffer");
        }
        buf
    }

    /// Report whether the version is a semver 1.0.0 or semver 2.0.0 version,
    /// according to Nuget's rules, unless the version has been overridden
    /// to force semver 2.0.
    ///
    /// ## Reference
    ///
    /// https://learn.microsoft.com/en-us/nuget/concepts/package-versioning?tabs=semver20sort#semantic-versioning-200
    fn kind(&self) -> VersionKind {
        if let Some(kind) = self.override_kind {
            return kind;
        }

        if let Some(suffix) = &self.label {
            if suffix.contains('.') {
                return VersionKind::Semver2;
            }
        }

        if self.build_meta.is_some() {
            return VersionKind::Semver2;
        }

        VersionKind::Semver1
    }
}

impl From<&Version> for Revision {
    fn from(version: &Version) -> Self {
        Self::from(version.to_string())
    }
}

impl TryFrom<String> for Version {
    type Error = NugetConstraintError;

    fn try_from(version: String) -> Result<Self, Self::Error> {
        version.as_str().try_into()
    }
}

impl FromStr for Version {
    type Err = NugetConstraintError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
    }
}

impl TryFrom<&str> for Version {
    type Error = NugetConstraintError;

    fn try_from(version: &str) -> Result<Self, Self::Error> {
        /// Parse a version segment into a number.
        macro_rules! parse {
            (@err_missing => $name:expr) => {
                || NugetConstraintError::VersionParse{
                    version: $name.to_string(),
                    message: "missing field".to_string()
                }
            };
            (@err_parse => $name:expr) => {
                |err| NugetConstraintError::VersionParse{
                    version: $name.to_string(),
                    message: format!("parse field '{}': {}", $name, err),
                }
            };
            ($name:expr, $part:expr) => {
                $part
                    .ok_or_else(parse!(@err_missing => $name))?
                    .parse()
                    .map_err(parse!(@err_parse => $name))
            };
            (optional => $name:expr, $part:expr) => {
                if let Some(part) = $part {
                    part.parse().map_err(parse!(@err_parse => $name)).map(Some)
                } else {
                    Ok(None)
                }
                .map(|inner| inner.unwrap_or_default())
            };
        }

        // In semver:
        // Build metadata MAY be denoted by appending a plus sign and a series of dot separated identifiers
        // immediately following the patch or pre-release version.
        let (version, build) = match version.rsplit_once('+') {
            None => (version, None),
            Some((version, metadata)) => (version, Some(metadata)),
        };

        // In semver:
        // A pre-release version MAY be denoted by appending a hyphen and a series of dot separated identifiers
        // immediately following the patch version.
        let (version, suffix) = match version.split_once('-') {
            None => (version, None),
            Some((version, suffix)) => (version, Some(suffix)),
        };

        let mut parts = version.split('.');
        Ok(Version {
            major: parse!("major", parts.next())?,
            minor: parse!(optional => "minor", parts.next())?,
            patch: parse!(optional => "patch", parts.next())?,
            revision: parse!(optional => "revision", parts.next())?,
            label: suffix.map(ToOwned::to_owned),
            build_meta: build.map(ToOwned::to_owned),
            override_kind: None,
        })
    }
}

impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Implements normalization as described in the Microsoft documentation:
        // https://learn.microsoft.com/en-us/nuget/concepts/package-versioning?tabs=semver20sort#normalized-version-numbers
        //
        // Specifically:
        // 1. `major.minor.patch` is always written
        // 2. `revision` is only written if provided
        // 3. Suffix and build is written if needed
        // 4. Extra zeroes (trailing or preceding) in any versions are removed
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)?;

        if self.revision > 0 {
            write!(f, ".{}", self.revision)?;
        }
        if let Some(ref suffix) = self.label {
            write!(f, "-{suffix}")?;
        }
        if let Some(ref build) = self.build_meta {
            write!(f, "+{build}")?;
        }
        Ok(())
    }
}

impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Version {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Convenience macro for short circuiting pure numeric comparison.
        macro_rules! cmp_numeric {
            ($a:expr, $b:expr) => {
                if let ord @ (Ordering::Greater | Ordering::Less) = $a.cmp($b) {
                    return ord;
                }
            };
        }

        // There are two ordering modes for Nuget: semver 1.0.0 and semver 2.0.0.
        // Reference: https://learn.microsoft.com/en-us/nuget/concepts/package-versioning?tabs=semver20sort#pre-release-versions
        //
        // For both, they first consider `major.minor.patch.revision`,
        // and order based on those (numerically),
        // just as in the semver spec.
        cmp_numeric!(self.major, &other.major);
        cmp_numeric!(self.minor, &other.minor);
        cmp_numeric!(self.patch, &other.patch);
        cmp_numeric!(self.revision, &other.revision);

        // Again, both order prereleases lower if the other doesn't have a prerelease.
        //
        // Where the two schemes differ is in how they handle prerelease labels.
        // Important reminder: build metadata does not affect order.
        //
        // In the docs for [`Version`], we wrote:
        // > Note that when using the `Ord` implementations,
        // > we can only compare two versions, so it's possible for a set that contains
        // > a mixture of [`VersionKind::Semver1`] and [`VersionKind::Semver2`] versions
        // > may have confusing sorts unless the kinds are normalized
        // > (using [`Version::override_kind`]).
        //
        // This part of the code is where this confusing sort behavior could manifest;
        // since we compare two versions at a time, it's possible that a collection
        // that has multiple of each kind of version could exhibit different sort
        // characteristics depending on where those versions happen to line up.
        match (&self.label, &other.label) {
            (None, Some(_)) => Ordering::Greater,
            (Some(_), None) => Ordering::Less,
            (None, None) => Ordering::Equal,
            (Some(a), Some(b)) if VersionKind::any(VersionKind::Semver2, [self, other]) => {
                // Semver 2.0 compares labels by separating each dot segment of the label,
                // then comparing them in order until a difference is found.
                let mut a_segments = a.split('.');
                let mut b_segments = b.split('.');
                loop {
                    // If one side has more segments than the other, but they're otherwise equal,
                    // the one with more segments sorts higher.
                    //
                    // This handles clause (4) of the precedence order defined in the semver 2.0 spec;
                    // it comes first in our code by the consequence that the clause
                    // "if all preceding identifiers are equal"
                    // must have been true for us to have gotten to the top of the loop.
                    //
                    // For the first iteration of the loop, "all preceding identifiers" can only mean
                    // "identifiers not in the label" (because we haven't looked at the label yet).
                    // Future iterations of the loop will bail early if an inequality is found.
                    let (a, b) = match (a_segments.next(), b_segments.next()) {
                        (Some(a), Some(b)) => (a, b),
                        (Some(_), None) => return Ordering::Greater,
                        (None, Some(_)) => return Ordering::Less,
                        (None, None) => break,
                    };

                    // If both segments can be parsed as numbers and aren't equal,
                    // this difference determines their relative order.
                    // We keep the numeric versions around becuase we'll reuse them later,
                    // no need to parse again.
                    //
                    // This handles clause (1) of the precendence order defined in the semver 2.0 spec.
                    let (a_numeric, b_numeric) = (a.parse::<usize>().ok(), b.parse::<usize>().ok());
                    if let (Some(a), Some(b)) = (a_numeric, b_numeric) {
                        if a != b {
                            return a.cmp(&b);
                        }
                    }

                    // Here we treat both segments as lowercase strings so that we can compare them case-insensitively.
                    // Per [Nuget documentation](https://learn.microsoft.com/en-us/nuget/concepts/package-versioning?tabs=semver20sort#where-nugetversion-diverges-from-semantic-versioning)
                    let (a, b) = (&a.to_lowercase(), &b.to_lowercase());
                    // If the segments aren't strictly numeric and aren't equal, we compare lexically.
                    // This handles clause (2) of the precendence order defined in the semver 2.0 spec.
                    if a != b {
                        return a.cmp(b);
                    }

                    // Nothing cleanly delineating these yet, so sort numeric identifiers lower, if possible.
                    // This handles clause (3) of the precendence order defined in the semver 2.0 spec.
                    match (a_numeric, b_numeric) {
                        (Some(_), None) => return Ordering::Less,
                        (None, Some(_)) => return Ordering::Greater,
                        _ => {}
                    }
                }

                // If we get all the way here, we didn't find any difference between the two.
                Ordering::Equal
            }
            (Some(self_label), Some(other_label)) => {
                // Semver 1.0 just compares labels in ascii order.
                // https://semver.org/spec/v1.0.0.html#spec-item-4
                self_label.to_lowercase().cmp(&other_label.to_lowercase())
            }
        }
    }
}

impl PartialEq for Version {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Version {}

/// Nuget supports both semver 1.0.0 and 2.0.0 semantics for version ordering.
///
/// Since we have to parse their total list of versions and perform ordering client side,
/// we have to implement this same functionality.
///
/// The default is the lowest-sorted variant.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
#[non_exhaustive]
pub enum VersionKind {
    /// The version should be compared according to Semver 2.0.0 rules.
    Semver2,

    /// The version should be compared according to Semver 1.0.0 rules.
    #[default]
    Semver1,
}

impl VersionKind {
    fn any<'a>(kind: Self, versions: impl IntoIterator<Item = &'a Version>) -> bool {
        versions.into_iter().any(|version| version.kind() == kind)
    }
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
pub fn parse_constraints(input: &str) -> Result<Vec<Constraint<Revision>>, NugetConstraintError> {
    fn operator(input: &str) -> IResult<&str, &str> {
        alt((tag("="), tag(">="), tag("<="), tag(">"), tag("<"))).parse(input)
    }

    fn version(input: &str) -> IResult<&str, &str> {
        take_while1(|c: char| c.is_alphanumeric() || c == '.' || c == '-' || c == '+').parse(input)
    }

    fn single_constraint(input: &str) -> IResult<&str, Constraint<Revision>> {
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

    fn constraints(input: &str) -> IResult<&str, Vec<Constraint<Revision>>> {
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
        .map_err(|e| NugetConstraintError::ConstraintParse {
            constraints: input.to_string(),
            message: format!("failed to parse constraint: {e:?}"),
        })
}

/// Errors from NuGet constraints
#[derive(Error, Clone, PartialEq, Eq, Debug)]
pub enum NugetConstraintError {
    /// Error when parsing a NuGet version
    #[error("parse version {version:?}: {message:?})")]
    VersionParse { 
        /// The version string that could not be parsed
        version: String, 
        /// The error message describing why parsing failed
        message: String 
    },

    /// Error when parsing constraints for NuGet
    #[error("parse constraints {constraints:?}: {message:?})")]
    ConstraintParse {
        /// The constraint string that could not be parsed
        constraints: String,
        /// The error message describing why parsing failed
        message: String,
    },
}

#[cfg(test)]
mod tests {
    use simple_test_case::test_case;

    use super::*;
    use crate::{constraint, constraints, Constraints};

    // Define a macro to create NuGet version constraints more easily
    macro_rules! nuget_version {
        ($major:expr, $minor:expr, $patch:expr) => {
            Version::try_from(format!("{}.{}.{}", $major, $minor, $patch)).unwrap()
        };
        ($major:expr, $minor:expr, $patch:expr, $revision:expr) => {
            Version::try_from(format!("{}.{}.{}.{}", $major, $minor, $patch, $revision)).unwrap()
        };
        ($version:expr) => {
            Version::try_from($version.to_string()).unwrap()
        };
    }

    #[test_case("= 1.0.0", constraints!({ Equal => nuget_version!(1, 0, 0) }); "1.0.0_eq_1.0.0")]
    #[test_case(
        ">= 1.0, < 2.0", 
        constraints!(
            { GreaterOrEqual => nuget_version!("1.0") },
            { Less => nuget_version!("2.0") }
        ); 
        "1.0_geq_1.0_AND_lt_2.0"
    )]
    #[test_case("> 1.0.0-alpha", constraints!({ Greater => nuget_version!("1.0.0-alpha") }); "1.0.0-alpha_gt_1.0.0-alpha")]
    #[test_case("<= 2.0.0", constraints!({ LessOrEqual => nuget_version!("2.0.0") }); "2.0.0_leq_2.0.0")]
    #[test_case("= 1.0.0.1", constraints!({ Equal => nuget_version!(1, 0, 0, 1) }); "1.0.0.1_eq_1.0.0.1")]
    #[test]
    fn test_nuget_constraints_parsing(input: &str, expected: Constraints<Version>) {
        // Parse the constraints string to Constraint<Revision>
        let parsed_revisions = parse_constraints(input).expect("should parse constraint");
        
        // Convert from Constraint<Revision> to Constraint<Version>
        let parsed_versions: Vec<Constraint<Version>> = parsed_revisions
            .into_iter()
            .map(|constraint| {
                constraint.map(|rev| Version::try_from(rev.to_string()).unwrap())
            })
            .collect();
        
        // Create Constraints from the vector
        let parsed = Constraints::from(parsed_versions);
        
        // Compare expected vs actual
        assert_eq!(
            format!("{:?}", expected), 
            format!("{:?}", parsed),
            "compare constraints: expected={:?}, parsed={:?}", expected, parsed
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

    #[test_case(constraint!(Equal => nuget_version!(1, 0, 0)), nuget_version!(1, 0, 0), true; "equal_versions")]
    #[test_case(constraint!(Equal => nuget_version!("1.0")), nuget_version!(1, 0, 0), true; "equal_normalized_versions")]
    #[test_case(constraint!(Equal => nuget_version!("1")), nuget_version!(1, 0, 0), true; "equal_normalized_versions_major")]
    #[test_case(constraint!(Equal => nuget_version!("1.0.0.0")), nuget_version!("1.0"), true; "equal_normalized_versions_with_zeros")]
    #[test_case(constraint!(Equal => nuget_version!(1, 0, 0)), nuget_version!(1, 0, 0, 1), false; "unequal_with_revision")]
    #[test_case(constraint!(Equal => nuget_version!("1.0.0-alpha")), nuget_version!("1.0.0-ALPHA"), true; "equal_case_insensitive_prerelease")]
    #[test_case(constraint!(GreaterOrEqual => nuget_version!(1, 0, 0)), nuget_version!(1, 0, 0), true; "greater_equal_same")]
    #[test_case(constraint!(GreaterOrEqual => nuget_version!(1, 0, 0)), nuget_version!(0, 9, 0), false; "not_greater_equal")]
    #[test_case(constraint!(Less => nuget_version!(2, 0, 0)), nuget_version!(1, 9, 9), true; "less_than")]
    #[test_case(constraint!(Less => nuget_version!(2, 0, 0)), nuget_version!(1, 9, 9, 9), true; "less_than_with_revision")]
    #[test_case(constraint!(Less => nuget_version!(2, 0, 0, 1)), nuget_version!(1, 9, 9, 9), true; "less_than_with_revisions")]
    #[test_case(constraint!(Less => nuget_version!("1.0")), nuget_version!("1"), false; "not_less_equal")]
    #[test_case(constraint!(Greater => nuget_version!("1.0.0-alpha")), nuget_version!(1, 0, 0), true; "release_greater_than_prerelease")]
    #[test_case(constraint!(Greater => nuget_version!("1.0.0-alpha")), nuget_version!("1.0.0-beta"), true; "beta_greater_than_alpha")]
    #[test_case(constraint!(Equal => nuget_version!("1.0.0+metadata")), nuget_version!(1, 0, 0), true; "ignore_metadata_in_comparison")]
    #[test_case(constraint!(Compatible => nuget_version!(1, 2, 3)), nuget_version!(1, 2, 4), true; "compatible_within_minor")]
    #[test_case(constraint!(Compatible => nuget_version!(1, 2, 3)), nuget_version!(1, 3, 0), false; "not_compatible_higher_minor")]
    #[test_case(constraint!(Compatible => nuget_version!(1, 2, 0)), nuget_version!(1, 9, 9), true; "compatible_within_major")]
    #[test_case(constraint!(Compatible => nuget_version!("1.0.0.0")), nuget_version!(1, 9, 9), true; "compatible_within_major_zeros")]
    #[test_case(constraint!(Compatible => nuget_version!(1, 2, 3, 4)), nuget_version!("1.2.3.9"), true; "compatible_within_patch")]
    #[test]
    fn test_nuget_version_comparison(constraint: Constraint<Version>, target: Version, expected: bool) {
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
    fn test_nuget_version_ordering(lhs: &str, rhs: &str, expected: Ordering) {
        let v1 = Version::try_from(lhs.to_string()).expect("valid version");
        let v2 = Version::try_from(rhs.to_string()).expect("valid version");
        assert_eq!(
            v1.cmp(&v2),
            expected,
            "Expected {lhs} to be {expected:?} {rhs}"
        );
    }
}