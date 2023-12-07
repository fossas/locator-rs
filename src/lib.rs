#![doc = include_str!("../README.md")]
#![deny(unsafe_code)]
#![deny(missing_docs)]
#![warn(rust_2018_idioms)]

use std::fmt::Display;

use getset::{CopyGetters, Getters};
use lazy_static::lazy_static;
use regex::Regex;
use serde::{Deserialize, Serialize};
use typed_builder::TypedBuilder;

mod error;
mod fetcher;
pub use error::*;
pub use fetcher::*;

/// Core, and most services that interact with Core,
/// refer to open source packages via the `Locator` type.
///
/// This type is nearly universally rendered to a string
/// before being serialized to the database or sent over the network.
///
/// This type represents a _validly-constructed_ `Locator`, but does not
/// validate whether a `Locator` is actually valid. This means that a
/// given `Locator` is guaranteed to be correctly formatted data,
/// but that the actual repository or revision to which the `Locator`
/// refers is _not_ guaranteed to exist or be accessible.
/// Currently the canonical method for validating whether a given `Locator` is
/// accessible is to run it through the Core fetcher system.
///
/// For more information on the background of `Locator` and fetchers generally,
/// FOSSA employees may refer to
/// [Fetchers and Locators](https://go/fetchers-doc).
#[derive(Clone, Eq, PartialEq, Hash, Debug, TypedBuilder, Getters, CopyGetters)]
pub struct Locator {
    /// Determines which fetcher is used to download this project.
    #[getset(get_copy = "pub")]
    fetcher: Fetcher,

    /// Specifies the organization ID to which this project is namespaced.
    #[builder(default, setter(strip_option))]
    #[getset(get_copy = "pub")]
    org_id: Option<usize>,

    /// Specifies the unique identifier for the project by fetcher.
    ///
    /// For example, the `git` fetcher fetching a github project
    /// uses a value in the form of `{user_name}/{project_name}`.
    #[builder(setter(transform = |project: impl ToString| project.to_string()))]
    #[getset(get = "pub")]
    project: String,

    /// Specifies the version for the project by fetcher.
    ///
    /// For example, the `git` fetcher fetching a github project
    /// uses a value in the form of `{git_sha}` or `{git_tag}`,
    /// and the fetcher disambiguates.
    #[builder(default, setter(transform = |revision: impl ToString| Some(revision.to_string())))]
    #[getset(get = "pub")]
    revision: Option<String>,
}

impl Locator {
    /// Parse a `Locator`.
    ///
    /// The input string must be in one of the following forms:
    /// - `{fetcher}+{project}`
    /// - `{fetcher}+{project}$`
    /// - `{fetcher}+{project}${revision}`
    ///
    /// Projects may also be namespaced to a specific organization;
    /// in such cases the organization ID is at the start of the `{project}` field
    /// separated by a slash. The ID can be any non-negative integer.
    /// This yields the following formats:
    /// - `{fetcher}+{org_id}/{project}`
    /// - `{fetcher}+{org_id}/{project}$`
    /// - `{fetcher}+{org_id}/{project}${revision}`
    ///
    /// This parse function is based on the function used in FOSSA Core for maximal compatibility.
    pub fn parse(locator: &str) -> Result<Self, Error> {
        lazy_static! {
            static ref RE: Regex = Regex::new(
                r"^(?:(?P<fetcher>[a-z-]+)\+|)(?P<project>[^$]+)(?:\$|)(?P<revision>.+|)$"
            )
            .expect("Locator parsing expression must compile");
        }

        let mut captures = RE.captures_iter(locator);
        let capture = captures.next().ok_or_else(|| ParseError::Syntax {
            input: locator.to_string(),
        })?;

        let fetcher =
            capture
                .name("fetcher")
                .map(|m| m.as_str())
                .ok_or_else(|| ParseError::Field {
                    input: locator.to_owned(),
                    field: "fetcher".to_string(),
                })?;

        let fetcher = Fetcher::try_from(fetcher).map_err(|error| ParseError::Fetcher {
            input: locator.to_owned(),
            fetcher: fetcher.to_string(),
            error,
        })?;

        let project = capture
            .name("project")
            .map(|m| m.as_str().to_owned())
            .ok_or_else(|| ParseError::Field {
                input: locator.to_owned(),
                field: "project".to_string(),
            })?;

        let revision = capture.name("revision").map(|m| m.as_str()).and_then(|s| {
            if s.is_empty() {
                None
            } else {
                Some(s.to_string())
            }
        });

        match parse_org_project(&project) {
            Ok((org_id @ Some(_), project)) => Ok(Locator {
                fetcher,
                org_id,
                project: String::from(project),
                revision,
            }),
            Ok((org_id @ None, _)) => Ok(Locator {
                fetcher,
                org_id,
                project,
                revision,
            }),
            Err(error) => Err(Error::Parse(ParseError::Project {
                input: locator.to_owned(),
                project,
                error,
            })),
        }
    }

    /// Converts the locator into a [`PackageLocator`] by discarding the `revision` component.
    /// Equivalent to the `From` implementation, but offered as a method for convenience.
    pub fn into_package(self) -> PackageLocator {
        self.into()
    }
}

impl Display for Locator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fetcher = &self.fetcher;
        write!(f, "{fetcher}+")?;

        let project = &self.project;
        if let Some(org_id) = &self.org_id {
            write!(f, "{org_id}/")?;
        }
        write!(f, "{project}")?;

        if let Some(revision) = &self.revision {
            write!(f, "${revision}")?;
        }

        Ok(())
    }
}

impl<'de> Deserialize<'de> for Locator {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        Locator::parse(&raw).map_err(serde::de::Error::custom)
    }
}

impl Serialize for Locator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

/// A [`Locator`] specialized to not include the `revision` component.
///
/// Any [`Locator`] may be converted to a `PackageLocator` by simply discarding the `revision` component.
/// To create a [`Locator`] from a `PackageLocator`, the value for `revision` must be provided; see [`Locator`] for details.
#[derive(Clone, Eq, PartialEq, Hash, Debug, TypedBuilder)]
pub struct PackageLocator {
    /// Determines which fetcher is used to download this dependency
    /// from the internet.
    fetcher: Fetcher,

    /// Specifies the organization ID to which this project is namespaced.
    org_id: Option<usize>,

    /// Specifies the unique identifier for the project by fetcher.
    ///
    /// For example, the `git` fetcher fetching a github project
    /// uses a value in the form of `{user_name}/{project_name}`.
    #[builder(setter(transform = |project: impl ToString| project.to_string()))]
    project: String,
}

impl PackageLocator {
    /// Parse a `PackageLocator`.
    ///
    /// The input string must be in one of the following forms:
    /// - `{fetcher}+{project}`
    /// - `{fetcher}+{project}$`
    /// - `{fetcher}+{project}${revision}`
    ///
    /// Projects may also be namespaced to a specific organization;
    /// in such cases the organization ID is at the start of the `{project}` field
    /// separated by a slash. The ID can be any non-negative integer.
    /// This yields the following formats:
    /// - `{fetcher}+{org_id}/{project}`
    /// - `{fetcher}+{org_id}/{project}$`
    /// - `{fetcher}+{org_id}/{project}${revision}`
    ///
    /// This parse function is based on the function used in FOSSA Core for maximal compatibility.
    ///
    /// This implementation ignores the `revision` segment if it exists. If this is not preferred, use [`Locator`] instead.
    pub fn parse(locator: &str) -> Result<Self, Error> {
        let full = Locator::parse(locator)?;
        Ok(Self {
            fetcher: full.fetcher,
            org_id: full.org_id,
            project: full.project,
        })
    }

    /// Promote a `PackageLocator` to a [`Locator`] by providing the value to use for the `revision` component.
    pub fn promote(self, revision: Option<String>) -> Locator {
        Locator {
            fetcher: self.fetcher,
            org_id: self.org_id,
            project: self.project,
            revision,
        }
    }
}

impl Display for PackageLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let converted = Locator::from(self);
        write!(f, "{converted}")
    }
}

impl<'de> Deserialize<'de> for PackageLocator {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        PackageLocator::parse(&raw).map_err(serde::de::Error::custom)
    }
}

impl Serialize for PackageLocator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

impl From<Locator> for PackageLocator {
    fn from(full: Locator) -> Self {
        Self {
            fetcher: full.fetcher,
            org_id: full.org_id,
            project: full.project,
        }
    }
}

impl From<PackageLocator> for Locator {
    fn from(package: PackageLocator) -> Self {
        Self {
            fetcher: package.fetcher,
            org_id: package.org_id,
            project: package.project,
            revision: None,
        }
    }
}

impl From<&PackageLocator> for Locator {
    fn from(package: &PackageLocator) -> Self {
        package.clone().into()
    }
}

/// Optionally parse an org ID and trimmed project out of a project string.
fn parse_org_project(project: &str) -> Result<(Option<usize>, &str), ProjectParseError> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^(?:(?P<org_id>\d+)/)?(?P<project>.+)")
            .expect("Project parsing expression must compile");
    }

    let mut captures = RE.captures_iter(project);
    let capture = captures.next().ok_or_else(|| ProjectParseError::Project {
        project: project.to_string(),
    })?;

    let trimmed_project =
        capture
            .name("project")
            .map(|m| m.as_str())
            .ok_or_else(|| ProjectParseError::Field {
                project: project.to_string(),
                field: String::from("project"),
            })?;

    // If we fail to parse the org_id as a valid number, don't fail the overall parse;
    // just don't namespace to org ID and return the input unmodified.
    match capture.name("org_id").map(|m| m.as_str()).map(str::parse) {
        // An org ID was provided and validly parsed, use it.
        Some(Ok(org_id)) => Ok((Some(org_id), trimmed_project)),

        // Otherwise, if we either didn't get an org ID section,
        // or it wasn't a valid org ID,
        // just use the project as-is.
        _ => Ok((None, project)),
    }
}

#[cfg(test)]
mod test;
