use std::str::FromStr;

use assert_matches::assert_matches;
use impls::impls;
use itertools::{Itertools, izip};
use pretty_assertions::assert_eq;
use serde::Deserialize;
use static_assertions::const_assert;

use locator::*;

#[test]
fn trait_impls() {
    const_assert!(impls!(StrictLocator: AsRef<StrictLocator>));
    const_assert!(impls!(StrictLocator: FromStr));
    const_assert!(impls!(StrictLocator: From<&'static StrictLocator>));
    const_assert!(impls!(StrictLocator: TryFrom<&'static Locator>));
    const_assert!(impls!(StrictLocator: From<StrictLocator>));
    const_assert!(impls!(StrictLocator: TryFrom<Locator>));
}

#[test]
fn parse_using_fromstr() {
    let input = "git+github.com/foo/bar$abcd";
    let parsed = input.parse().expect("must parse locator");
    let expected = strict!(Git, "github.com/foo/bar", "abcd");
    assert_eq!(expected, parsed);
    assert_eq!(&parsed.to_string(), input);
}

#[test]
fn parse_render_successful() {
    let input = "git+github.com/foo/bar$abcd";
    let parsed = StrictLocator::parse(input).expect("must parse locator");
    let expected = strict!(Git, "github.com/foo/bar", "abcd");
    assert_eq!(expected, parsed);
    assert_eq!(&parsed.to_string(), input);
}

#[test]
fn parse_invalid_ecosystem() {
    let input = "foo+github.com/foo/bar";
    let parsed = StrictLocator::parse(input);
    assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
}

#[test]
fn parse_missing_package() {
    let input = "git+";
    let parsed = StrictLocator::parse(input);
    assert_matches!(parsed, Err(Error::Parse(ParseError::Syntax { .. })));
}

#[test]
fn parse_missing_revision() {
    let input = "git+package";
    let parsed = StrictLocator::parse(input);
    assert_matches!(parsed, Err(Error::Parse(ParseError::Field { .. })));
}

#[test]
fn parse_empty_revision() {
    let input = "git+package$";
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
    let revisions = ["1", "1234abcd1234"];

    for (ecosystem, org, package, revision) in izip!(ecosystems, orgs, packages, revisions) {
        let input = format!("{ecosystem}+{org}/{package}${revision}");
        let Ok(parsed) = StrictLocator::parse(&input) else {
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
        assert_eq!(
            parsed.revision().as_str(),
            revision,
            "'revision' in '{input}' must match",
        );
    }
}

#[test]
fn render_with_org() {
    let locator = strict!(org 1234 => Custom, "foo/bar", "123abc");
    let rendered = locator.to_string();
    assert_eq!("custom+1234/foo/bar$123abc", rendered);
}

#[test]
fn render_with_revision() {
    let locator = strict!(Custom, "foo/bar", "123abc");
    let rendered = locator.to_string();
    assert_eq!("custom+foo/bar$123abc", rendered);
}

#[test]
fn roundtrip_serialization() {
    let input = strict!(org 1 => Custom, "foo", "bar");
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
    let locator = strict!(org 1 => Custom, "foo", "bar");

    let expected = Test { locator };
    let deserialized = serde_json::from_str(input).expect("must deserialize");
    assert_eq!(expected, deserialized, "deserialize {input}");
}

#[test]
fn demotes_package() {
    let input = strict!(org 1 => Custom, "foo", "bar");
    let expected = package!(org 1 => Custom, "foo");
    let demoted = input.clone().into();
    assert_eq!(expected, demoted, "demote {input}");
}

#[test]
fn demotes_locator() {
    let input = strict!(org 1 => Custom, "foo", "bar");
    let expected = locator!(org 1 => Custom, "foo", "bar");
    let demoted = input.clone().into();
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
        "custom+baz$1234",
        "custom+1/bam$1234",
        "custom+2/bam$1234",
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
