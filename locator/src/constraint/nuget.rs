//! # NuGet Requirements and Constraints
//!
//! Implements NuGet-specific version parsing, comparison, and constraint checking.
//! This module enables accurate evaluation of version constraints for NuGet packages
//! following their unique versioning conventions.
//!
//! ## Key Features
//!
//! - **4-part versioning**: Supports NuGet's optional revision number (`Major.Minor.Patch.Revision`)
//! - **SemVer 1.0/2.0 distinction**: Correctly identifies and handles both versioning schemes
//! - **Normalization**: Implements NuGet's version normalization rules (e.g., `1.0`, `1.0.0`, and `1.0.0.0` are equivalent)
//! - **Case-insensitive comparisons**: Prerelease labels are compared case-insensitively
//! - **Compatible ranges**: Implements NuGet's specific compatibility rules for version ranges
//!
//! ## Compatibility Rules
//!
//! NuGet has specific rules for what versions are considered compatible:
//!
//! - For versions with revision > 0: `[version, next-patch)` (e.g., `1.2.3.4` is compatible with `[1.2.3.4, 1.2.4.0)`)
//! - For versions with patch > 0: `[version, next-minor)` (e.g., `1.2.3` is compatible with `[1.2.3, 1.3.0.0)`)
//! - For versions with minor > 0: `[version, next-major)` (e.g., `1.2.0` is compatible with `[1.2.0, 2.0.0.0)`)
//! - For versions `1.0.0`: `[version, next-major)` (e.g., `1.0.0` is compatible with `[1.0.0, 2.0.0.0)`)
//!
//! ## SemVer Identification
//!
//! NuGet considers a version to be SemVer 2.0.0 when either:
//!
//! - The pre-release label is dot-separated (e.g., `1.0.0-alpha.1`)
//! - The version has build metadata (e.g., `1.0.0+githash`)
//!
//! Otherwise, it follows SemVer 1.0.0 rules for comparison.
//!
//! ## Integration with Constraint System
//!
//! This module fully integrates with the [`Comparable`](super::Comparable) trait system,
//! enabling NuGet versions to be used with the generic [`Constraint`](super::Constraint)
//! and [`Constraints`](crate::Constraints) types.
//!
//! ## References
//!
//! - [NuGet SemVer 1/2 rules](https://learn.microsoft.com/en-us/nuget/concepts/package-versioning?tabs=semver20sort#semantic-versioning-200)
//! - [NuGet Requirementing](https://learn.microsoft.com/en-us/nuget/concepts/package-versioning)
//! - [Requirement Normalization](https://learn.microsoft.com/en-us/nuget/concepts/package-versioning?tabs=semver20sort#normalized-version-numbers)
//! - [GHSA API](https://docs.github.com/en/rest/security-advisories/global-advisories)
//! - [OSV ranges](https://ossf.github.io/osv-schema/#affectedranges-field)

use std::{cmp::Ordering, fmt::Write, str::FromStr};

use bon::Builder;
use compact_str::ToCompactString;
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

/// Implementation of the Comparable trait to enable NuGet versions to be used in constraints.
///
/// This implementation allows NuGet versions to be used as the basis for constraints
/// that can be checked against general [`Revision`] types. It follows the constraint
/// matching semantics where the constraint (self) defines the condition that the
/// target version (rev) must satisfy.
///
/// Note that the comparison appears reversed in functions like `less()` because
/// of how constraints work:
/// - For a constraint like "< 2.0.0", we check if the target version is less than 2.0.0
/// - So if the target is 1.9.9, we verify 1.9.9 < 2.0.0
///
/// This approach ensures consistent constraint behavior across all package ecosystems.
impl Comparable<Revision> for Requirement {
    /// Compatibility check for NuGet versions, following NuGet's convention.
    /// For NuGet, the compatible operator (~=) works as follows:
    /// - If any of minor, patch, or revision is > 0, increment the leftmost non-zero component
    ///   and set all components to the right to zero.
    /// - Target version must be between [min_version, max_version)
    fn compatible(&self, rev: &Revision) -> bool {
        if let Ok(target) = Requirement::try_from(rev.to_string()) {
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
        if let Ok(target) = Requirement::try_from(rev.to_string()) {
            // NuGet versions are equal if they're the same after normalization
            // (e.g., 1.0 == 1.0.0 == 1.0.0.0)
            self == &target
        } else {
            false
        }
    }

    /// Less than check for NuGet versions
    fn less(&self, rev: &Revision) -> bool {
        if let Ok(target) = Requirement::try_from(rev.to_string()) {
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
        if let Ok(target) = Requirement::try_from(rev.to_string()) {
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
        if let Ok(target) = Requirement::try_from(rev.to_string()) {
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
        if let Ok(target) = Requirement::try_from(rev.to_string()) {
            // For a constraint like ">= 1.0.0", the target version must be greater than or equal to the constraint version

            // Compare by cloning to avoid reference issues
            let self_clone = self.clone();
            target >= self_clone
        } else {
            false
        }
    }
}

/// Implementation of the Comparable trait for direct NuGet version-to-version comparison.
///
/// This enables comparing a NuGet version constraint against another NuGet version
/// without first converting to a generic Revision. The implementation maintains the
/// same constraint matching semantics as the Revision variant, but with better
/// performance since no conversion is needed.
///
/// This implementation is particularly useful for internal version comparisons where
/// we already have proper NuGet version objects.
impl Comparable<Requirement> for Requirement {
    fn compatible(&self, other: &Requirement) -> bool {
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

    fn equal(&self, other: &Requirement) -> bool {
        self == other
    }

    fn less(&self, other: &Requirement) -> bool {
        // Create clones to handle value comparison
        let self_clone = self.clone();
        let other_clone = other.clone();
        other_clone < self_clone
    }

    fn greater(&self, other: &Requirement) -> bool {
        // Create clones to handle value comparison
        let self_clone = self.clone();
        let other_clone = other.clone();
        other_clone > self_clone
    }

    fn not_equal(&self, other: &Requirement) -> bool {
        self != other
    }

    fn less_or_equal(&self, other: &Requirement) -> bool {
        // Create clones to handle value comparison
        let self_clone = self.clone();
        let other_clone = other.clone();
        other_clone <= self_clone
    }

    fn greater_or_equal(&self, other: &Requirement) -> bool {
        // Create clones to handle value comparison
        let self_clone = self.clone();
        let other_clone = other.clone();
        other_clone >= self_clone
    }
}

/// Implementation of the Comparable trait for Revision-to-NuGet-Requirement comparison.
///
/// This enables bidirectional comparison between NuGet versions and generic Revisions.
/// The implementation attempts to parse the Revision into a NuGet Requirement first, then
/// delegates to the Requirement-to-Requirement comparison if successful.
///
/// Bidirectional comparison is essential for the constraint system to work correctly
/// regardless of which side (constraint or target) contains the NuGet version. This
/// ensures all constraint operations work seamlessly across different version types.
impl Comparable<Requirement> for Revision {
    fn compatible(&self, other: &Requirement) -> bool {
        match Requirement::try_from(self.to_string()) {
            Ok(self_version) => self_version.compatible(other),
            Err(_) => false,
        }
    }

    fn equal(&self, other: &Requirement) -> bool {
        match Requirement::try_from(self.to_string()) {
            Ok(self_version) => self_version.equal(other),
            Err(_) => false,
        }
    }

    fn not_equal(&self, other: &Requirement) -> bool {
        !self.equal(other)
    }

    fn less(&self, other: &Requirement) -> bool {
        match Requirement::try_from(self.to_string()) {
            Ok(self_version) => self_version.less(other),
            Err(_) => false,
        }
    }

    fn greater(&self, other: &Requirement) -> bool {
        match Requirement::try_from(self.to_string()) {
            Ok(self_version) => self_version.greater(other),
            Err(_) => false,
        }
    }

    fn less_or_equal(&self, other: &Requirement) -> bool {
        match Requirement::try_from(self.to_string()) {
            Ok(self_version) => self_version.less_or_equal(other),
            Err(_) => false,
        }
    }

    fn greater_or_equal(&self, other: &Requirement) -> bool {
        match Requirement::try_from(self.to_string()) {
            Ok(self_version) => self_version.greater_or_equal(other),
            Err(_) => false,
        }
    }
}

// Note:
// Much of this was brought over from `sparkle`.
// At some point after we've rolled this out, we will want to deduplicate this code.

/// Represents a NuGet package version requirement with all components and metadata.
///
/// This struct fully implements NuGet's versioning system, supporting both SemVer 1.0.0 and 2.0.0
/// with NuGet-specific extensions like the optional fourth version segment (Revision).
///
/// ## Requirement Formats
///
/// NuGet supports various requirement formats:
/// - Requirement ranges: `>= 1.0.0, < 2.0.0`
/// - Exact versions: `= 1.0.0`
/// - Implicit equality: `1.0.0` (equivalent to `= 1.0.0`)
/// - Requirement with build metadata: `1.0.0+githash`
///
/// ## Requirement Normalization
///
/// NuGet treats version requirements differently based on the presence or absence of certain components:
/// - Missing segments are treated as if they were zero (`1.0` = `1.0.0` = `1.0.0.0`)
/// - Only major version is required (others default to 0)
/// - The `Display` implementation follows NuGet normalization rules
///
/// ## Prerelease and Build Metadata
///
/// NuGet version requirements may contain:
/// - Prerelease labels (`-alpha`, `-beta.1`, etc.)
/// - Build metadata (`+build.123`, `+githash`, etc.)
///
/// Build metadata is ignored in all comparison operations, as per SemVer spec.
///
/// ## SemVer Requirement Handling
///
/// NuGet automatically detects and applies different comparison rules based on whether a version
/// follows SemVer 1.0.0 or 2.0.0. For collections with mixed version kinds, use
/// [`Requirement::override_kind`] to ensure consistent comparison behavior.
///
/// ## Reference
///
/// - [NuGet Package Requirementing](https://learn.microsoft.com/en-us/nuget/concepts/package-versioning)
//
// Note that `Requirement` effectively contains a generic semver2 and semver1 parser and comparator,
// it just has extra semantics from Nuget. When we parse other version requirements in the future
// we can start thinking about how to abstract this to reduce duplicate logic.
//
// I didn't go ahead and do this now because I didn't want to inflict premature
// abstractions on us; I'm not sure what other specific changes in semantics other
// package managers will require.
//
// Most likely this'll look like "extract shared primitives and use them
// when defining other package-manager-specific requirement types",
// but it's _possible_ we may be able to model this in traits as some form
// of extensions to the base parsing/comparison rules.
#[derive(Clone, Debug, Builder, Derivative)]
#[derivative(Hash)]
#[non_exhaustive]
pub struct Requirement {
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
    override_kind: Option<RequirementKind>,
}

impl Requirement {
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
    fn kind(&self) -> RequirementKind {
        if let Some(kind) = self.override_kind {
            return kind;
        }

        if let Some(suffix) = &self.label {
            if suffix.contains('.') {
                return RequirementKind::Semver2;
            }
        }

        if self.build_meta.is_some() {
            return RequirementKind::Semver2;
        }

        RequirementKind::Semver1
    }
}

impl TryFrom<&Requirement> for Revision {
    type Error = crate::Error;

    fn try_from(value: &Requirement) -> Result<Self, Self::Error> {
        Self::try_from(value.to_string())
    }
}

impl TryFrom<String> for Requirement {
    type Error = NugetConstraintError;

    fn try_from(version: String) -> Result<Self, Self::Error> {
        version.as_str().try_into()
    }
}

impl TryFrom<&String> for Requirement {
    type Error = NugetConstraintError;

    fn try_from(version: &String) -> Result<Self, Self::Error> {
        version.as_str().try_into()
    }
}

impl FromStr for Requirement {
    type Err = NugetConstraintError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
    }
}

impl TryFrom<&str> for Requirement {
    type Error = NugetConstraintError;

    fn try_from(version: &str) -> Result<Self, Self::Error> {
        /// Parse a version segment into a number.
        macro_rules! parse {
            (@err_missing => $name:expr) => {
                || NugetConstraintError::RequirementParse{
                    version: $name.to_string(),
                    message: "missing field".to_string()
                }
            };
            (@err_parse => $name:expr) => {
                |err| NugetConstraintError::RequirementParse{
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
        Ok(Requirement {
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

impl std::fmt::Display for Requirement {
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

impl PartialOrd for Requirement {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Requirement {
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
        // In the docs for [`Requirement`], we wrote:
        // > Note that when using the `Ord` implementations,
        // > we can only compare two versions, so it's possible for a set that contains
        // > a mixture of [`RequirementKind::Semver1`] and [`RequirementKind::Semver2`] versions
        // > may have confusing sorts unless the kinds are normalized
        // > (using [`Requirement::override_kind`]).
        //
        // This part of the code is where this confusing sort behavior could manifest;
        // since we compare two versions at a time, it's possible that a collection
        // that has multiple of each kind of version could exhibit different sort
        // characteristics depending on where those versions happen to line up.
        match (&self.label, &other.label) {
            (None, Some(_)) => Ordering::Greater,
            (Some(_), None) => Ordering::Less,
            (None, None) => Ordering::Equal,
            (Some(a), Some(b)) if RequirementKind::any(RequirementKind::Semver2, [self, other]) => {
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

impl PartialEq for Requirement {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Requirement {}

/// Determines which SemVer rules to apply when comparing NuGet versions.
///
/// NuGet has two different comparison modes based on whether it detects a version
/// as SemVer 1.0.0 or 2.0.0. This enum allows controlling and overriding the
/// detection logic when needed.
///
/// The key difference between these modes is how prerelease identifiers are compared:
/// - SemVer 1.0.0: Simple string comparison of the entire prerelease label
/// - SemVer 2.0.0: Segment-by-segment comparison, with special rules for numeric vs. string segments
///
/// For collections containing both types, consider normalizing to a consistent
/// kind to prevent unexpected sorting behavior.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
#[non_exhaustive]
pub enum RequirementKind {
    /// The version should be compared according to Semver 2.0.0 rules.
    Semver2,

    /// The version should be compared according to Semver 1.0.0 rules.
    #[default]
    Semver1,
}

impl RequirementKind {
    fn any<'a>(kind: Self, versions: impl IntoIterator<Item = &'a Requirement>) -> bool {
        versions.into_iter().any(|version| version.kind() == kind)
    }
}

/// Parses a NuGet version constraint string into [`Constraint<Revision>`] objects.
///
/// This parser is specifically designed to handle version constraints from security
/// vulnerability databases like the GitHub Security Advisory API. The constraints
/// follow a standard format used across the ecosystem for expressing affected
/// version ranges.
///
/// # Supported Formats
///
/// - Individual constraints with operators: `= 1.0.0`, `>= 2.0.0`, `< 3.0.0`
/// - Compound ranges with multiple constraints: `>= 1.0.0, < 2.0.0`
///
/// Each constraint is converted to the appropriate [`Constraint`] variant containing
/// the version as a [`Revision`]. This allows for generic constraint checking
/// across different package ecosystems.
///
/// # Integration
///
/// This function bridges between external version range formats and the internal
/// constraint system. The resulting constraints can be used with the [`Comparable`]
/// trait implementations to check if specific versions match the constraints.
///
/// # Data Sources
///
/// These constraint strings typically come from:
/// - [GHSA Global Advisories API](https://docs.github.com/en/rest/security-advisories/global-advisories)
/// - [OSV vulnerability data](https://ossf.github.io/osv-schema/#affectedranges-field)
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

        let rev = Revision::Opaque(ver.to_compact_string());

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

/// Represents errors that can occur when working with NuGet versions and constraints.
///
/// These error types provide detailed information about what went wrong during version
/// parsing or constraint evaluation, helping users understand and fix issues in their
/// version specifications.
#[derive(Error, Clone, PartialEq, Eq, Debug)]
pub enum NugetConstraintError {
    /// Error when parsing a NuGet version
    #[error("parse version {version:?}: {message:?})")]
    RequirementParse {
        /// The version string that could not be parsed
        version: String,
        /// The error message describing why parsing failed
        message: String,
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
