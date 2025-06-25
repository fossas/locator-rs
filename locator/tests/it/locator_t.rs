use std::str::FromStr;

use assert_matches::assert_matches;
use impls::impls;
use itertools::{Itertools, izip};
use pretty_assertions::assert_eq;
use proptest::prelude::*;
use serde::Deserialize;
use static_assertions::const_assert;

use locator::*;

#[test]
fn trait_impls() {
    const_assert!(impls!(Locator: AsRef<Locator>));
    const_assert!(impls!(Locator: FromStr));
    const_assert!(impls!(Locator: From<&'static Locator>));
    const_assert!(impls!(Locator: From<&'static StrictLocator>));
    const_assert!(impls!(Locator: From<&'static PackageLocator>));
    const_assert!(impls!(Locator: From<StrictLocator>));
    const_assert!(impls!(Locator: From<PackageLocator>));
}

#[test]
fn parse_using_fromstr() {
    let input = "git+github.com/foo/bar";
    let parsed = input.parse().expect("must parse locator");
    let expected = locator!(Git, "github.com/foo/bar");
    assert_eq!(expected, parsed);
    assert_eq!(&parsed.to_string(), input);

    let input = "git+github.com/foo/bar$1234";
    let parsed = input.parse().expect("must parse locator");
    let expected = locator!(Git, "github.com/foo/bar", "1234");
    assert_eq!(expected, parsed);
    assert_eq!(&parsed.to_string(), input);
}

#[test]
fn parse_render_successful() {
    let input = "git+github.com/foo/bar";
    let parsed = Locator::parse(input).expect("must parse locator");
    let expected = locator!(Git, "github.com/foo/bar");
    assert_eq!(expected, parsed);
    assert_eq!(&parsed.to_string(), input);

    let input = "git+github.com/foo/bar$abcd";
    let parsed = Locator::parse(input).expect("must parse locator");
    let expected = locator!(Git, "github.com/foo/bar", "abcd");
    assert_eq!(expected, parsed);
    assert_eq!(&parsed.to_string(), input);
}

#[test]
fn parse_invalid_ecosystem() {
    let input = "foo+github.com/foo/bar";
    let parsed = Locator::parse(input);
    assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
}

#[test]
fn parse_missing_package() {
    let input = "git+";
    let parsed = Locator::parse(input);
    assert_matches!(parsed, Err(Error::Parse(ParseError::Syntax { .. })));
}

#[test]
fn parse_invalid_syntax() {
    let input = "";
    let parsed = Locator::parse(input);
    assert_matches!(parsed, Err(Error::Parse(ParseError::Syntax { .. })));

    let input = "git+$";
    let parsed = Locator::parse(input);
    assert_matches!(parsed, Err(Error::Parse(ParseError::Syntax { .. })));
}

#[test]
fn parse_with_org() {
    let ecosystems = Ecosystem::iter().map(|ecosystem| format!("{ecosystem}"));
    let orgs = [
        OrgId::from(0usize),
        OrgId::from(1),
        OrgId::from(1234),
        OrgId::from(2385028),
        OrgId::from(19847938492847928usize),
    ];
    let packages = ["github.com/foo/bar", "some-name"];
    let revisions = ["", "$", "$1", "$1234abcd1234"];

    for (ecosystem, org, package, revision) in izip!(ecosystems, orgs, packages, revisions) {
        let input = format!("{ecosystem}+{org}/{package}{revision}");
        let Ok(parsed) = Locator::parse(&input) else {
            panic!("must parse '{input}'")
        };

        assert_eq!(
            parsed.ecosystem().to_string(),
            ecosystem,
            "'ecosystem' in '{input}' must match"
        );
        assert_eq!(
            parsed.organization(),
            Some(org),
            "'organization' in '{input}' must match"
        );
        assert_eq!(
            parsed.package().as_str(),
            package,
            "'package' in '{input}' must match"
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
    let locator = locator!(org 1234 => Custom, "foo/bar", "123abc");
    let rendered = locator.to_string();
    assert_eq!("custom+1234/foo/bar$123abc", rendered);

    let package_only = PackageLocator::from(locator);
    let rendered = package_only.to_string();
    assert_eq!("custom+1234/foo/bar", rendered);
}

#[test]
fn render_with_revision() {
    let locator = locator!(Custom, "foo/bar", "123abc");
    let rendered = locator.to_string();
    assert_eq!("custom+foo/bar$123abc", rendered);
}

#[test]
fn render_package() {
    let locator = locator!(Custom, "foo/bar");
    let rendered = locator.to_string();
    assert_eq!("custom+foo/bar", rendered);
}

#[test]
fn roundtrip_serialization() {
    let input = locator!(org 1 => Custom, "foo", "bar");
    let serialized = serde_json::to_string(&input).expect("must serialize");
    let deserialized = serde_json::from_str(&serialized).expect("must deserialize");
    assert_eq!(input, deserialized);
}

#[test]
fn roundtrip_fromparts() {
    let input = locator!(org 1 => Custom, "foo", "bar");
    let parts = input.clone().into_parts();
    let constructed = Locator::from_parts(parts);
    assert_eq!(input, constructed);
}

#[test]
fn serde_deserialization() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Test {
        locator: Locator,
    }

    let input = r#"{ "locator": "custom+1/foo$bar" }"#;
    let expected = locator!(org 1 => Custom, "foo", "bar");
    let expected = Test { locator: expected };

    let deserialized = serde_json::from_str(input).expect("must deserialize");
    assert_eq!(expected, deserialized, "deserialize {input}");
}

#[test]
fn demotes() {
    let input = locator!(org 1 => Custom, "foo", "bar");
    let expected = package!(org 1 => Custom, "foo");
    let demoted = input.clone().into();
    assert_eq!(expected, demoted, "demote {input}");
}

#[test]
fn promotes_strict() {
    let input = locator!(org 1 => Custom, "foo", "1234");
    let expected = strict!(org 1 => Custom, "foo", "1234");
    let promoted = input.clone().try_into().expect("promotes locator");
    assert_eq!(expected, promoted, "promote {input}");
}

#[test]
fn build_with_macro() {
    locator::locator!(org 10 => Npm, "lodash", "1.0");
    locator::locator!(Npm, "lodash", "1.0");
    locator::locator!(Npm, "lodash");
}

#[test]
fn build_with_builder() {
    locator::Locator::builder()
        .organization(10)
        .ecosystem(Ecosystem::Npm)
        .package("lodash")
        .revision(locator::revision!("1.0"))
        .build();
    locator::Locator::builder()
        .ecosystem(Ecosystem::Npm)
        .package("lodash")
        .revision(locator::revision!("1.0"))
        .build();
    locator::Locator::builder()
        .ecosystem(Ecosystem::Npm)
        .package("lodash")
        .build();
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
/// - Contains at least one character that is not a control character, space, or the literal `$`
/// - Contains a literal `$`
/// - Contains at least one character that is not a control character, space, or the literal `$`
const VALID_INPUTS_GIT: &str = r"git\+[^\pC\s$]+\$[^\pC\s$]+";

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
/// - Contains at least one character that is not a control character, space, or the literal `$`
/// - Contains a literal `$`
/// - Contains at least one character that is not a control character, space, or the literal `$`
const VALID_INPUTS_GIT_WITH_ORG: &str = r"git\+\d*/[^\pC\s$]+\$[^\pC\s$]+";

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
/// - Contains at least one character that is not a control character, space, or the literal `$`
/// - Contains a literal `$`
/// - Contains at least one character that is not a control character, space, or the literal `$`
const VALID_INPUTS_CUSTOM: &str = r"custom\+[^\pC\s$]+\$[^\pC\s$]+";

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
/// - Contains zero or more digits that do not begin with `0`
/// - Contains a literal `/`
/// - Contains at least one character that is not a control character, space, or the literal `$`
/// - Contains a literal `$`
/// - Contains at least one character that is not a control character, space, or the literal `$`
const VALID_INPUTS_CUSTOM_WITH_ORG: &str = r"custom\+([1-9]\d*)?/[^\pC\s$]+\$[^\pC\s$]+";

proptest! {
    /// Tests randomly generated strings that match the provided regular expression against the parser.
    /// Validates that the parser succeeds by converting the locator back into a string again.
    #[test]
    fn parses_arbitrary_locator_custom_with_org(input in VALID_INPUTS_CUSTOM_WITH_ORG) {
        let parsed = Locator::parse(&input).expect("must parse locator");
        assert_eq!(parsed.to_string(), input);
    }
}
