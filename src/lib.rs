#![doc = include_str!("../README.md")]
#![deny(unsafe_code)]
#![deny(missing_docs)]
#![warn(rust_2018_idioms)]

use lazy_static::lazy_static;
use regex::Regex;

mod error;
mod fetcher;
mod locator;
mod locator_package;
mod locator_strict;

pub use error::*;
pub use fetcher::*;

pub use locator::*;
pub use locator_package::*;
pub use locator_strict::*;

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
mod tests {
    use itertools::izip;

    use super::*;

    #[test]
    fn parses_org_project() {
        let orgs = [0usize, 1, 9809572];
        let names = ["name", "name/foo"];

        for (org, name) in izip!(orgs, names) {
            let test = format!("{org}/{name}");
            let Ok((Some(org_id), project)) = parse_org_project(&test) else {
                panic!("must parse '{test}'")
            };
            assert_eq!(org_id, org, "'org_id' must match in '{test}'");
            assert_eq!(project, name, "'project' must match in '{test}");
        }
    }

    #[test]
    fn parses_org_project_no_org() {
        let names = ["/name/foo", "/name", "abcd/1234/name", "1abc2/name"];
        for test in names {
            let Ok((org_id, project)) = parse_org_project(test) else {
                panic!("must parse '{test}'")
            };
            assert_eq!(org_id, None, "'org_id' must be None in '{test}'");
            assert_eq!(project, test, "'project' must match in '{test}");
        }
    }
}
