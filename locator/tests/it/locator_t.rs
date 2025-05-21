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
fn from_existing() {
    let first = locator!(Git, "github.com/foo/bar");
    let second = Locator::builder()
        .ecosystem(first.ecosystem())
        .maybe_org_id(first.org_id())
        .package(first.package())
        .maybe_revision(first.revision().as_ref())
        .build();
    assert_eq!(first, second);
}

#[test]
fn optional_fields() {
    let with_options = Locator::builder()
        .ecosystem(Ecosystem::Git)
        .package("github.com/foo/bar")
        .maybe_org_id(Some(1234))
        .maybe_revision(Some("abcd"))
        .build();
    let expected = Locator::builder()
        .ecosystem(Ecosystem::Git)
        .package("github.com/foo/bar")
        .org_id(1234)
        .revision("abcd")
        .build();
    assert_eq!(expected, with_options);

    let without_options = Locator::builder()
        .ecosystem(Ecosystem::Git)
        .package("github.com/foo/bar")
        .maybe_org_id(None::<usize>)
        .maybe_revision(None::<&str>)
        .build();
    let expected = Locator::builder()
        .ecosystem(Ecosystem::Git)
        .package("github.com/foo/bar")
        .build();
    assert_eq!(expected, without_options);
}

#[test]
fn trait_impls() {
    const_assert!(impls!(Locator: AsRef<Locator>));
    const_assert!(impls!(Locator: FromStr));
    const_assert!(impls!(Locator: From<StrictLocator>));
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
    let expected = Locator::builder()
        .ecosystem(Ecosystem::Git)
        .package("github.com/foo/bar")
        .build();
    assert_eq!(expected, parsed);
    assert_eq!(&parsed.to_string(), input);

    let input = "git+github.com/foo/bar$abcd";
    let parsed = Locator::parse(input).expect("must parse locator");
    let expected = Locator::builder()
        .ecosystem(Ecosystem::Git)
        .package("github.com/foo/bar")
        .revision("abcd")
        .build();
    assert_eq!(expected, parsed);
    assert_eq!(&parsed.to_string(), input);
}

#[test]
fn parse_invalid_ecosystem() {
    let input = "foo+github.com/foo/bar";
    let parsed = Locator::parse(input);
    assert_matches!(parsed, Err(Error::Parse(ParseError::Ecosystem { .. })));
}

#[test]
fn parse_missing_package() {
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
            parsed.org_id(),
            Some(org),
            "'org_id' in '{input}' must match"
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
    let locator = Locator::builder()
        .ecosystem(Ecosystem::Custom)
        .org_id(1234)
        .package("foo/bar")
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
        .ecosystem(Ecosystem::Custom)
        .package("foo/bar")
        .revision("123abc")
        .build();

    let rendered = locator.to_string();
    assert_eq!("custom+foo/bar$123abc", rendered);
}

#[test]
fn render_package() {
    let locator = Locator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo/bar")
        .build();

    let rendered = locator.to_string();
    assert_eq!("custom+foo/bar", rendered);
}

#[test]
fn roundtrip_serialization() {
    let input = Locator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
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
        .ecosystem(Ecosystem::Custom)
        .package("foo")
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
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .revision("abcd")
        .build();

    let expected = PackageLocator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .build();
    let demoted = input.clone().into_package();
    assert_eq!(expected, demoted, "demote {input}");
}

#[test]
fn promotes_strict() {
    let input = Locator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .build();

    let expected = StrictLocator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .revision("bar")
        .build();
    let promoted = input.clone().promote_strict("bar");
    assert_eq!(expected, promoted, "promote {input}");
}

#[test]
fn promotes_strict_existing() {
    let input = Locator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .revision("1234")
        .org_id(1)
        .build();

    let expected = StrictLocator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .revision("1234")
        .build();

    let promoted = input.clone().promote_strict("bar");
    assert_eq!(expected, promoted, "promote {input}");
}

#[test]
fn promotes_strict_existing_function() {
    let input = Locator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .build();

    let expected = StrictLocator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .revision("bar")
        .build();

    let promoted = input.clone().promote_strict_with(|| String::from("bar"));
    assert_eq!(expected, promoted, "promote {input}");
}

#[test]
fn promotes_strict_existing_lazy() {
    let input = Locator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .revision("1234")
        .org_id(1)
        .build();

    let expected = StrictLocator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .revision("1234")
        .build();

    let promoted = input
        .clone()
        .promote_strict_with(|| panic!("should not be called"));
    assert_eq!(expected, promoted, "promote {input}");
}

#[test]
fn try_promote_strict_with_revision() {
    let input = Locator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .revision("1234")
        .org_id(1)
        .build();

    let expected = StrictLocator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .revision("1234")
        .build();

    let result = input.try_promote_strict().expect("must promote strict");
    assert_eq!(expected, result);
}

#[test]
fn try_promote_strict_without_revision() {
    let input = Locator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .build();

    input
        .try_promote_strict()
        .expect_err("must fail to promote");
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
