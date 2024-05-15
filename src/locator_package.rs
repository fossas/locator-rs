use std::{cmp::Ordering, fmt::Display};

use getset::{CopyGetters, Getters};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use typed_builder::TypedBuilder;

use crate::{Error, Fetcher, Locator, StrictLocator};

/// A [`Locator`] specialized to not include the `revision` component.
///
/// Any [`Locator`] may be converted to a `PackageLocator` by simply discarding the `revision` component.
/// To create a [`Locator`] from a `PackageLocator`, the value for `revision` must be provided; see [`Locator`] for details.
#[derive(Clone, Eq, PartialEq, Hash, Debug, TypedBuilder, Getters, CopyGetters, JsonSchema)]
pub struct PackageLocator {
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
    /// This implementation ignores the `revision` segment if it exists. If this is not preferred, use [`Locator`] instead.
    pub fn parse(locator: &str) -> Result<Self, Error> {
        let full = Locator::parse(locator)?;
        Ok(full.into_package())
    }

    /// Promote a `PackageLocator` to a [`Locator`] by providing the value to use for the `revision` component.
    pub fn promote(self, revision: Option<String>) -> Locator {
        let locator = Locator::builder()
            .fetcher(self.fetcher)
            .project(self.project);

        match (self.org_id, revision) {
            (None, None) => locator.build(),
            (None, Some(revision)) => locator.revision(revision).build(),
            (Some(org_id), None) => locator.org_id(org_id).build(),
            (Some(org_id), Some(revision)) => locator.org_id(org_id).revision(revision).build(),
        }
    }

    /// Promote a `PackageLocator` to a [`StrictLocator`] by providing the value to use for the `revision` component.
    pub fn promote_strict(self, revision: impl ToString) -> StrictLocator {
        let locator = StrictLocator::builder()
            .fetcher(self.fetcher)
            .project(self.project)
            .revision(revision);

        match self.org_id {
            None => locator.build(),
            Some(org_id) => locator.org_id(org_id).build(),
        }
    }

    /// Explodes the locator into its (owned) parts.
    /// Used for conversions without cloning.
    pub(crate) fn explode(self) -> (Fetcher, Option<usize>, String) {
        (self.fetcher, self.org_id, self.project)
    }
}

impl Ord for PackageLocator {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.fetcher.cmp(&other.fetcher) {
            Ordering::Equal => {}
            ord => return ord,
        }
        alphanumeric_sort::compare_str(&self.project, &other.project)
    }
}

impl PartialOrd for PackageLocator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
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
        let (fetcher, org_id, project, _) = full.explode();
        Self {
            fetcher,
            org_id,
            project,
        }
    }
}

impl From<&Locator> for PackageLocator {
    fn from(full: &Locator) -> Self {
        Self {
            fetcher: full.fetcher(),
            org_id: full.org_id(),
            project: full.project().clone(),
        }
    }
}

impl From<StrictLocator> for PackageLocator {
    fn from(strict: StrictLocator) -> Self {
        let (fetcher, org_id, project, _) = strict.explode();
        Self {
            fetcher,
            org_id,
            project,
        }
    }
}

impl From<&StrictLocator> for PackageLocator {
    fn from(strict: &StrictLocator) -> Self {
        Self {
            fetcher: strict.fetcher(),
            org_id: strict.org_id(),
            project: strict.project().clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use itertools::{izip, Itertools};
    use pretty_assertions::assert_eq;
    use serde::Deserialize;
    use strum::IntoEnumIterator;

    use crate::ParseError;

    use super::*;

    #[test]
    fn parse_render_successful() {
        let input = "git+github.com/foo/bar";
        let parsed = PackageLocator::parse(input).expect("must parse locator");
        let expected = PackageLocator::builder()
            .fetcher(Fetcher::Git)
            .project("github.com/foo/bar")
            .build();
        assert_eq!(expected, parsed);
        assert_eq!(&parsed.to_string(), input);
    }

    #[test]
    fn parse_drops_revision() {
        let input = "git+github.com/foo/bar$abcd";
        let parsed = PackageLocator::parse(input).expect("must parse locator");
        let expected = PackageLocator::builder()
            .fetcher(Fetcher::Git)
            .project("github.com/foo/bar")
            .build();
        assert_eq!(expected, parsed);
    }

    #[test]
    fn parse_invalid_fetcher() {
        let input = "foo+github.com/foo/bar";
        let parsed = PackageLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Fetcher { .. })));
    }

    #[test]
    fn parse_missing_project() {
        let input = "git+";
        let parsed = PackageLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
    }

    #[test]
    fn parse_invalid_syntax() {
        let input = "";
        let parsed = PackageLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Syntax { .. })));

        let input = "git+$";
        let parsed = PackageLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
    }

    #[test]
    fn parse_with_org() {
        let fetchers = Fetcher::iter().map(|fetcher| format!("{fetcher}"));
        let orgs = [0usize, 1, 1234, 2385028, 19847938492847928];
        let projects = ["github.com/foo/bar", "some-name"];
        let revisions = ["", "$", "$1", "$1234abcd1234"];

        for (fetcher, org, project, revision) in izip!(fetchers, orgs, projects, revisions) {
            let input = format!("{fetcher}+{org}/{project}{revision}");
            let Ok(parsed) = PackageLocator::parse(&input) else {
                panic!("must parse '{input}'")
            };

            assert_eq!(
                parsed.fetcher().to_string(),
                fetcher,
                "'fetcher' in '{input}' must match"
            );
            assert_eq!(
                parsed.org_id(),
                Some(org),
                "'org_id' in '{input}' must match"
            );
            assert_eq!(
                parsed.project().as_str(),
                project,
                "'project' in '{input}' must match"
            );
        }
    }

    #[test]
    fn render_with_org() {
        let locator = PackageLocator::builder()
            .fetcher(Fetcher::Custom)
            .org_id(1234)
            .project("foo/bar")
            .build();

        let rendered = locator.to_string();
        assert_eq!("custom+1234/foo/bar", rendered);
    }

    #[test]
    fn roundtrip_serialization() {
        let input = PackageLocator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .org_id(1)
            .build();

        let serialized = serde_json::to_string(&input).expect("must serialize");
        let deserialized = serde_json::from_str(&serialized).expect("must deserialize");
        assert_eq!(input, deserialized);
    }

    #[test]
    fn serde_deserialization() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Test {
            locator: PackageLocator,
        }

        let input = r#"{ "locator": "custom+1/foo" }"#;
        let locator = PackageLocator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .org_id(1)
            .build();

        let expected = Test { locator };
        let deserialized = serde_json::from_str(input).expect("must deserialize");
        assert_eq!(expected, deserialized, "deserialize {input}");
    }

    #[test]
    fn promotes_locator() {
        let input = PackageLocator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .org_id(1)
            .build();

        let expected = Locator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .org_id(1)
            .build();
        let promoted = input.clone().promote(None);
        assert_eq!(expected, promoted, "promote {input}");

        let expected = Locator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .org_id(1)
            .revision("bar")
            .build();
        let promoted = input.clone().promote(Some(String::from("bar")));
        assert_eq!(expected, promoted, "promote {input}");
    }

    #[test]
    fn promotes_strict() {
        let input = PackageLocator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .org_id(1)
            .build();

        let expected = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .org_id(1)
            .revision("bar")
            .build();
        let promoted = input.clone().promote_strict("bar");
        assert_eq!(expected, promoted, "promote {input}");
    }

    #[test]
    fn ordering() {
        let locators = vec![
            "git+github.com/foo/bar",
            "git+github.com/foo/bar$1234",
            "custom+baz$1234",
            "custom+1/bam$1234",
            "custom+2/bam$1234",
        ]
        .into_iter()
        .map(PackageLocator::parse)
        .collect::<Result<Vec<_>, _>>()
        .expect("must parse locators");

        let expected = vec![
            "custom+1/bam",
            "custom+2/bam",
            "custom+baz",
            "git+github.com/foo/bar",
            "git+github.com/foo/bar",
        ];
        let sorted = locators
            .iter()
            .sorted()
            .map(PackageLocator::to_string)
            .collect_vec();
        assert_eq!(expected, sorted, "sort {locators:?}");
    }
}