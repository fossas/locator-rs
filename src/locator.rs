use std::fmt::Display;

use documented::Documented;
use getset::{CopyGetters, Getters};
use lazy_static::lazy_static;
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::json;
use typed_builder::TypedBuilder;
use utoipa::{
    openapi::{ObjectBuilder, SchemaType},
    ToSchema,
};

use crate::{
    parse_org_project, Error, Fetcher, OrgId, PackageLocator, ParseError, Project, Revision,
    StrictLocator,
};

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
///
/// ## Ordering
///
/// Locators order by:
/// 1. Fetcher, alphanumerically.
/// 2. Organization ID, alphanumerically; missing organizations are sorted higher.
/// 3. The project field, alphanumerically.
/// 4. The revision field:
///    If both comparing locators use semver, these are compared using semver rules;
///    otherwise these are compared alphanumerically.
///    Missing revisions are sorted higher.
///
/// Importantly, there may be other metrics for ordering using the actual code host
/// which contains the package (for example, ordering by release date).
/// This library does not perform such ordering.
///
/// ## Parsing
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
#[derive(
    Clone,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Debug,
    TypedBuilder,
    Getters,
    CopyGetters,
    Documented,
)]
pub struct Locator {
    /// Determines which fetcher is used to download this project.
    #[getset(get_copy = "pub")]
    fetcher: Fetcher,

    /// Specifies the organization ID to which this project is namespaced.
    #[builder(default, setter(transform = |id: usize| Some(OrgId(id))))]
    #[getset(get_copy = "pub")]
    org_id: Option<OrgId>,

    /// Specifies the unique identifier for the project by fetcher.
    ///
    /// For example, the `git` fetcher fetching a github project
    /// uses a value in the form of `{user_name}/{project_name}`.
    #[builder(setter(transform = |project: impl ToString| Project(project.to_string())))]
    #[getset(get = "pub")]
    project: Project,

    /// Specifies the version for the project by fetcher.
    ///
    /// For example, the `git` fetcher fetching a github project
    /// uses a value in the form of `{git_sha}` or `{git_tag}`,
    /// and the fetcher disambiguates.
    #[builder(default, setter(transform = |revision: impl ToString| Some(Revision::from(revision.to_string()))))]
    #[getset(get = "pub")]
    revision: Option<Revision>,
}

impl Locator {
    /// Parse a `Locator`.
    /// For details, see the parsing section on [`Locator`].
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
                Some(Revision::from(s))
            }
        });

        match parse_org_project(&project) {
            Ok((org_id @ Some(_), project)) => Ok(Locator {
                fetcher,
                org_id,
                project,
                revision,
            }),
            Ok((org_id @ None, _)) => Ok(Locator {
                fetcher,
                org_id,
                project: Project::from(project.as_str()),
                revision,
            }),
            Err(error) => Err(Error::Parse(ParseError::Project {
                input: locator.to_owned(),
                project,
                error,
            })),
        }
    }

    /// Promote a `Locator` to a [`StrictLocator`] by providing the default value to use
    /// for the `revision` component, if one is not specified in the locator already.
    ///
    /// The `ToString` implementation is lazily evaluated if the locator doesn't already contain a revision.
    pub fn promote_strict(self, revision: impl ToString) -> StrictLocator {
        let locator = StrictLocator::builder()
            .fetcher(self.fetcher)
            .project(self.project)
            .revision(
                self.revision
                    .unwrap_or_else(|| Revision::from(revision.to_string())),
            );

        match self.org_id {
            None => locator.build(),
            Some(OrgId(id)) => locator.org_id(id).build(),
        }
    }

    /// Promote a `Locator` to a [`StrictLocator`] by providing the default value to use
    /// for the `revision` component, if one is not specified in the locator already.
    ///
    /// The revision is lazily evaluated if the locator doesn't already contain a revision.
    pub fn promote_strict_with<F: Fn() -> String>(self, revision: F) -> StrictLocator {
        let locator = StrictLocator::builder()
            .fetcher(self.fetcher)
            .project(self.project)
            .revision(self.revision.unwrap_or_else(|| Revision::from(revision())));

        match self.org_id {
            None => locator.build(),
            Some(OrgId(id)) => locator.org_id(id).build(),
        }
    }

    /// Converts the locator into a [`PackageLocator`] by discarding the `revision` component.
    /// Equivalent to the `From` implementation, but offered as a method for convenience.
    pub fn into_package(self) -> PackageLocator {
        self.into()
    }

    /// Explodes the locator into its (owned) parts.
    /// Used for conversions without cloning.
    pub(crate) fn explode(self) -> (Fetcher, Option<OrgId>, Project, Option<Revision>) {
        (self.fetcher, self.org_id, self.project, self.revision)
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

impl<'a> ToSchema<'a> for Locator {
    fn schema() -> (
        &'a str,
        utoipa::openapi::RefOr<utoipa::openapi::schema::Schema>,
    ) {
        (
            "Locator",
            ObjectBuilder::new()
                .description(Some(Self::DOCS))
                .example(Some(json!("git+github.com/fossas/example$1234")))
                .min_length(Some(3))
                .schema_type(SchemaType::String)
                .build()
                .into(),
        )
    }
}

impl From<PackageLocator> for Locator {
    fn from(package: PackageLocator) -> Self {
        let (fetcher, org_id, project) = package.explode();
        Self {
            fetcher,
            org_id,
            project,
            revision: None,
        }
    }
}

impl From<&PackageLocator> for Locator {
    fn from(package: &PackageLocator) -> Self {
        Self {
            fetcher: package.fetcher(),
            org_id: package.org_id(),
            project: package.project().clone(),
            revision: None,
        }
    }
}

impl From<StrictLocator> for Locator {
    fn from(strict: StrictLocator) -> Self {
        let (fetcher, org_id, project, revision) = strict.explode();
        Self {
            fetcher,
            org_id,
            project,
            revision: Some(revision),
        }
    }
}

impl From<&StrictLocator> for Locator {
    fn from(strict: &StrictLocator) -> Self {
        Self {
            fetcher: strict.fetcher(),
            org_id: strict.org_id(),
            project: strict.project().clone(),
            revision: Some(strict.revision().clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use assert_matches::assert_matches;
    use itertools::{izip, Itertools};
    use pretty_assertions::assert_eq;
    use proptest::prelude::*;
    use serde::Deserialize;
    use strum::IntoEnumIterator;

    use super::*;

    #[test]
    fn parse_render_successful() {
        let input = "git+github.com/foo/bar";
        let parsed = Locator::parse(input).expect("must parse locator");
        let expected = Locator::builder()
            .fetcher(Fetcher::Git)
            .project("github.com/foo/bar")
            .build();
        assert_eq!(expected, parsed);
        assert_eq!(&parsed.to_string(), input);

        let input = "git+github.com/foo/bar$abcd";
        let parsed = Locator::parse(input).expect("must parse locator");
        let expected = Locator::builder()
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
        let parsed = Locator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Fetcher { .. })));
    }

    #[test]
    fn parse_missing_project() {
        let input = "git+";
        let parsed = Locator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
    }

    #[test]
    fn parse_invalid_syntax() {
        let input = "";
        let parsed = Locator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Syntax { .. })));

        let input = "git+$";
        let parsed = Locator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
    }

    #[test]
    fn parse_with_org() {
        let fetchers = Fetcher::iter().map(|fetcher| format!("{fetcher}"));
        let orgs = [
            OrgId(0usize),
            OrgId(1),
            OrgId(1234),
            OrgId(2385028),
            OrgId(19847938492847928),
        ];
        let projects = ["github.com/foo/bar", "some-name"];
        let revisions = ["", "$", "$1", "$1234abcd1234"];

        for (fetcher, org, project, revision) in izip!(fetchers, orgs, projects, revisions) {
            let input = format!("{fetcher}+{org}/{project}{revision}");
            let Ok(parsed) = Locator::parse(&input) else {
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

            let revision = if revision.is_empty() || revision == "$" {
                None
            } else {
                Some(Cow::Borrowed(revision))
            };
            assert_eq!(
                parsed.revision().as_ref().map(|r| r.as_str()),
                revision,
                "'revision' in '{input}' must match",
            );
        }
    }

    #[test]
    fn render_with_org() {
        let locator = Locator::builder()
            .fetcher(Fetcher::Custom)
            .org_id(1234)
            .project("foo/bar")
            .revision("123abc")
            .build();

        let rendered = locator.to_string();
        assert_eq!("custom+1234/foo/bar$123abc", rendered);

        let package_only = locator.into_package();
        let rendered = package_only.to_string();
        assert_eq!("custom+1234/foo/bar", rendered);
    }

    #[test]
    fn render_with_revision() {
        let locator = Locator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo/bar")
            .revision("123abc")
            .build();

        let rendered = locator.to_string();
        assert_eq!("custom+foo/bar$123abc", rendered);
    }

    #[test]
    fn render_project() {
        let locator = Locator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo/bar")
            .build();

        let rendered = locator.to_string();
        assert_eq!("custom+foo/bar", rendered);
    }

    #[test]
    fn roundtrip_serialization() {
        let input = Locator::builder()
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
            locator: Locator,
        }

        let input = r#"{ "locator": "custom+1/foo$bar" }"#;
        let expected = Locator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .revision("bar")
            .org_id(1)
            .build();
        let expected = Test { locator: expected };

        let deserialized = serde_json::from_str(input).expect("must deserialize");
        assert_eq!(expected, deserialized, "deserialize {input}");
    }

    #[test]
    fn demotes() {
        let input = Locator::builder()
            .fetcher(Fetcher::Custom)
            .project("foo")
            .org_id(1)
            .revision("abcd")
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
    fn promotes_strict() {
        let input = Locator::builder()
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
            "custom+2/bam",
        ]
        .into_iter()
        .map(Locator::parse)
        .collect::<Result<Vec<_>, _>>()
        .expect("must parse locators");

        let expected = vec![
            "custom+baz$1234",
            "custom+1/bam$1234",
            "custom+2/bam",
            "custom+2/bam$1234",
            "git+github.com/foo/bar",
            "git+github.com/foo/bar$1234",
        ];
        let sorted = locators
            .iter()
            .sorted()
            .map(Locator::to_string)
            .collect_vec();
        assert_eq!(expected, sorted, "sort {locators:?}");
    }

    /// Regular expression that matches any unicode string that is:
    /// - Prefixed with `git+`
    /// - Contains at least one character that is not a control character and not the literal `$`
    /// - Contains a literal `$`
    /// - Contains at least one character that is not a control character and not the literal `$`
    const VALID_INPUTS_GIT: &str = r"git\+[^\pC$]+\$[^\pC$]+";

    proptest! {
        /// Tests randomly generated strings that match the provided regular expression against the parser.
        /// Validates that the parser succeeds by converting the locator back into a string again.
        #[test]
        fn parses_arbitrary_locator_git(input in VALID_INPUTS_GIT) {
            let parsed = Locator::parse(&input).expect("must parse locator");
            assert_eq!(parsed.to_string(), input);
        }
    }

    /// Regular expression that matches any unicode string that is:
    /// - Prefixed with `git+`
    /// - Contains zero or more digits
    /// - Contains a literal `/`
    /// - Contains at least one character that is not a control character and not the literal `$`
    /// - Contains a literal `$`
    /// - Contains at least one character that is not a control character and not the literal `$`
    const VALID_INPUTS_GIT_WITH_ORG: &str = r"git\+\d*/[^\pC$]+\$[^\pC$]+";

    proptest! {
        /// Tests randomly generated strings that match the provided regular expression against the parser.
        /// Validates that the parser succeeds by converting the locator back into a string again.
        #[test]
        fn parses_arbitrary_locator_git_with_org(input in VALID_INPUTS_GIT_WITH_ORG) {
            let parsed = Locator::parse(&input).expect("must parse locator");
            assert_eq!(parsed.to_string(), input);
        }
    }

    /// Regular expression that matches any unicode string that is:
    /// - Prefixed with `custom+`
    /// - Contains at least one character that is not a control character and not the literal `$`
    /// - Contains a literal `$`
    /// - Contains at least one character that is not a control character and not the literal `$`
    const VALID_INPUTS_CUSTOM: &str = r"custom\+[^\pC$]+\$[^\pC$]+";

    proptest! {
        /// Tests randomly generated strings that match the provided regular expression against the parser.
        /// Validates that the parser succeeds by converting the locator back into a string again.
        #[test]
        fn parses_arbitrary_locator_custom(input in VALID_INPUTS_CUSTOM) {
            let parsed = Locator::parse(&input).expect("must parse locator");
            assert_eq!(parsed.to_string(), input);
        }
    }

    /// Regular expression that matches any unicode string that is:
    /// - Prefixed with `custom+`
    /// - Contains zero or more digits
    /// - Contains a literal `/`
    /// - Contains at least one character that is not a control character and not the literal `$`
    /// - Contains a literal `$`
    /// - Contains at least one character that is not a control character and not the literal `$`
    const VALID_INPUTS_CUSTOM_WITH_ORG: &str = r"custom\+\d*/[^\pC$]+\$[^\pC$]+";

    proptest! {
        /// Tests randomly generated strings that match the provided regular expression against the parser.
        /// Validates that the parser succeeds by converting the locator back into a string again.
        #[test]
        fn parses_arbitrary_locator_custom_with_org(input in VALID_INPUTS_CUSTOM_WITH_ORG) {
            let parsed = Locator::parse(&input).expect("must parse locator");
            assert_eq!(parsed.to_string(), input);
        }
    }
}
