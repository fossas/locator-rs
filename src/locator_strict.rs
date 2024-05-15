use std::{borrow::Cow, cmp::Ordering, fmt::Display};

use getset::{CopyGetters, Getters};
use indoc::indoc;
use schemars::{
    schema::{InstanceType, Metadata, SchemaObject, StringValidation},
    JsonSchema,
};
use serde::{Deserialize, Serialize};
use serde_json::json;
use typed_builder::TypedBuilder;

use crate::{Error, Fetcher, Locator, PackageLocator, ParseError};

/// A [`Locator`] specialized to **require** the `revision` component.
#[derive(Clone, Eq, PartialEq, Hash, Debug, TypedBuilder, Getters, CopyGetters)]
pub struct StrictLocator {
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
    #[builder(setter(transform = |revision: impl ToString| revision.to_string()))]
    #[getset(get = "pub")]
    revision: String,
}

impl StrictLocator {
    /// Parse a `StrictLocator`.
    ///
    /// The input string must be in the following format:
    /// ```ignore
    /// {fetcher}+{project}${revision}
    /// ```
    ///
    /// Projects may also be namespaced to a specific organization;
    /// in such cases the organization ID is at the start of the `{project}` field
    /// separated by a slash. The ID can be any non-negative integer.
    /// This yields the following format:
    /// ```ignore
    /// {fetcher}+{org_id}/{project}${revision}
    /// ```
    pub fn parse(locator: &str) -> Result<Self, Error> {
        let (fetcher, org_id, project, revision) = Locator::parse(locator)?.explode();

        let Some(revision) = revision else {
            return Err(Error::Parse(ParseError::Field {
                input: locator.to_owned(),
                field: String::from("revision"),
            }));
        };

        Ok(Self {
            fetcher,
            org_id,
            project,
            revision,
        })
    }

    /// Converts the instance into a [`PackageLocator`] by discarding the `revision` component.
    /// Equivalent to the `From` implementation, but offered as a method for convenience.
    pub fn into_package(self) -> PackageLocator {
        self.into()
    }

    /// Converts the instance into a [`Locator`].
    /// Equivalent to the `From` implementation, but offered as a method for convenience.
    pub fn into_locator(self) -> Locator {
        self.into()
    }

    /// Explodes the locator into its (owned) parts.
    /// Used for conversions without cloning.
    pub(crate) fn explode(self) -> (Fetcher, Option<usize>, String, String) {
        (self.fetcher, self.org_id, self.project, self.revision)
    }
}

impl Ord for StrictLocator {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.fetcher.cmp(&other.fetcher) {
            Ordering::Equal => {}
            ord => return ord,
        }
        match alphanumeric_sort::compare_str(&self.project, &other.project) {
            Ordering::Equal => {}
            ord => return ord,
        }
        alphanumeric_sort::compare_str(&self.revision, &other.revision)
    }
}

impl PartialOrd for StrictLocator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Display for StrictLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fetcher = &self.fetcher;
        write!(f, "{fetcher}+")?;

        if let Some(org_id) = &self.org_id {
            write!(f, "{org_id}/")?;
        }

        let project = &self.project;
        let revision = &self.revision;
        write!(f, "{project}${revision}")?;

        Ok(())
    }
}

impl<'de> Deserialize<'de> for StrictLocator {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        StrictLocator::parse(&raw).map_err(serde::de::Error::custom)
    }
}

impl Serialize for StrictLocator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

impl JsonSchema for StrictLocator {
    fn schema_name() -> String {
        String::from("StrictLocator")
    }

    fn schema_id() -> Cow<'static, str> {
        // Include the module, in case a type with the same name is in another module/crate
        Cow::Borrowed(concat!(module_path!(), "::StrictLocator"))
    }

    fn json_schema(_: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        SchemaObject {
            instance_type: Some(InstanceType::String.into()),
            format: None,
            string: Some(Box::new(StringValidation {
                min_length: Some(3),
                max_length: None,
                pattern: Some(r"^[a-z-]+\+[^$]+\$.+$".to_string()),
            })),
            metadata: Some(Box::new(Metadata {
                description: Some(
                    indoc! {"
                        The input string must be in the following format:
                        ```
                        {fetcher}+{project}${revision}
                        ```

                        Projects may also be namespaced to a specific organization;
                        in such cases the organization ID is at the start of the `{project}` field
                        separated by a slash. The ID can be any non-negative integer.
                        This yields the following format:
                        ```
                        {fetcher}+{org_id}/{project}${revision}
                        ```
                    "}
                    .to_string(),
                ),
                examples: vec![json!("git+github.com/fossas/example$1234")],
                ..Default::default()
            })),
            ..Default::default()
        }
        .into()
    }

    fn is_referenceable() -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use itertools::{izip, Itertools};
    use pretty_assertions::assert_eq;
    use serde::Deserialize;
    use strum::IntoEnumIterator;

    use super::*;

    #[test]
    fn parse_render_successful() {
        let input = "git+github.com/foo/bar$abcd";
        let parsed = StrictLocator::parse(input).expect("must parse locator");
        let expected = StrictLocator::builder()
            .fetcher(Fetcher::Git)
            .project("github.com/foo/bar")
            .revision("abcd")
            .build();
        assert_eq!(expected, parsed);
        assert_eq!(&parsed.to_string(), input);
    }

    #[test]
    fn parse_invalid_fetcher() {
        let input = "foo+github.com/foo/bar";
        let parsed = StrictLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Fetcher { .. })));
    }

    #[test]
    fn parse_missing_project() {
        let input = "git+";
        let parsed = StrictLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
    }

    #[test]
    fn parse_missing_revision() {
        let input = "git+project";
        let parsed = StrictLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
    }

    #[test]
    fn parse_invalid_syntax() {
        let input = "";
        let parsed = StrictLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Syntax { .. })));

        let input = "git+$";
        let parsed = StrictLocator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
    }

    #[test]
    fn parse_with_org() {
        let fetchers = Fetcher::iter().map(|fetcher| format!("{fetcher}"));
        let orgs = [0usize, 1, 1234, 2385028, 19847938492847928];
        let projects = ["github.com/foo/bar", "some-name"];
        let revisions = ["1", "1234abcd1234"];

        for (fetcher, org, project, revision) in izip!(fetchers, orgs, projects, revisions) {
            let input = format!("{fetcher}+{org}/{project}${revision}");
            let Ok(parsed) = StrictLocator::parse(&input) else {
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
            assert_eq!(
                parsed.revision().as_str(),
                revision,
                "'revision' in '{input}' must match",
            );
        }
    }

    #[test]
    fn render_with_org() {
        let locator = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .org_id(1234)
            .project("foo/bar")
            .revision("123abc")
            .build();

        let rendered = locator.to_string();
        assert_eq!("custom+1234/foo/bar$123abc", rendered);
    }

    #[test]
    fn render_with_revision() {
        let locator = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo/bar")
            .revision("123abc")
            .build();

        let rendered = locator.to_string();
        assert_eq!("custom+foo/bar$123abc", rendered);
    }

    #[test]
    fn roundtrip_serialization() {
        let input = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .revision("bar")
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
            locator: StrictLocator,
        }

        let input = r#"{ "locator": "custom+1/foo$bar" }"#;
        let locator = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .revision("bar")
            .org_id(1)
            .build();

        let expected = Test { locator };
        let deserialized = serde_json::from_str(input).expect("must deserialize");
        assert_eq!(expected, deserialized, "deserialize {input}");
    }

    #[test]
    fn demotes_package() {
        let input = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .revision("bar")
            .org_id(1)
            .build();

        let expected = PackageLocator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .org_id(1)
            .build();
        let demoted = input.clone().into_package();
        assert_eq!(expected, demoted, "demote {input}");
    }

    #[test]
    fn demotes_locator() {
        let input = StrictLocator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .revision("bar")
            .org_id(1)
            .build();

        let expected = Locator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .org_id(1)
            .revision("bar")
            .build();
        let demoted = input.clone().into_locator();
        assert_eq!(expected, demoted, "demote {input}");
    }

    #[test]
    fn ordering() {
        let locators = vec![
            "git+github.com/foo/bar$abcd10",
            "git+github.com/foo/bar$abcd11",
            "custom+baz$1234",
            "custom+1/bam$1234",
            "custom+2/bam$1234",
        ]
        .into_iter()
        .map(StrictLocator::parse)
        .collect::<Result<Vec<_>, _>>()
        .expect("must parse locators");

        let expected = vec![
            "custom+1/bam$1234",
            "custom+2/bam$1234",
            "custom+baz$1234",
            "git+github.com/foo/bar$abcd10",
            "git+github.com/foo/bar$abcd11",
        ];
        let sorted = locators
            .iter()
            .sorted()
            .map(StrictLocator::to_string)
            .collect_vec();
        assert_eq!(expected, sorted, "sort {locators:?}");
    }
}
