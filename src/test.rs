use assert_matches::assert_matches;
use itertools::izip;
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
    let orgs = [0usize, 1, 1234, 2385028, 19847938492847928];
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
            Some(revision)
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

    let package_only = locator.into_package();
    let rendered = package_only.to_string();
    assert_eq!("custom+foo/bar", rendered);
}

#[test]
fn render_project() {
    let locator = Locator::builder()
        .fetcher(Fetcher::Custom)
        .project("foo/bar")
        .build();

    let rendered = locator.to_string();
    assert_eq!("custom+foo/bar", rendered);

    let package_only = locator.into_package();
    let rendered = package_only.to_string();
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
/// - Contains a literal `/`
/// - Contains zero or more digits
/// - Contains a literal `/`
/// - Contains at least one character that is not a control character and not the literal `$`
/// - Contains a literal `$`
/// - Contains at least one character that is not a control character and not the literal `$`
const VALID_INPUTS_GIT_WITH_ORG: &str = r"git\+/\d*/[^\pC$]+\$[^\pC$]+";

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
/// - Contains a literal `/`
/// - Contains zero or more digits
/// - Contains a literal `/`
/// - Contains at least one character that is not a control character and not the literal `$`
/// - Contains a literal `$`
/// - Contains at least one character that is not a control character and not the literal `$`
const VALID_INPUTS_CUSTOM_WITH_ORG: &str = r"custom\+/\d*/[^\pC$]+\$[^\pC$]+";

proptest! {
    /// Tests randomly generated strings that match the provided regular expression against the parser.
    /// Validates that the parser succeeds by converting the locator back into a string again.
    #[test]
    fn parses_arbitrary_locator_custom_with_org(input in VALID_INPUTS_CUSTOM_WITH_ORG) {
        let parsed = Locator::parse(&input).expect("must parse locator");
        assert_eq!(parsed.to_string(), input);
    }
}

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
