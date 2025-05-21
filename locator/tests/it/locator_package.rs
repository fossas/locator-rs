use std::str::FromStr;

use assert_matches::assert_matches;
use impls::impls;
use itertools::{Itertools, izip};
use pretty_assertions::assert_eq;
use serde::Deserialize;
use static_assertions::const_assert;

use locator::*;

#[test]
fn from_existing() {
    let first = package!(Git, "github.com/foo/bar");
    let second = PackageLocator::builder()
        .ecosystem(first.ecosystem())
        .maybe_org_id(first.org_id())
        .package(first.package())
        .build();
    assert_eq!(first, second);
}

#[test]
fn optional_fields() {
    let with_options = PackageLocator::builder()
        .ecosystem(Ecosystem::Git)
        .package("github.com/foo/bar")
        .maybe_org_id(Some(1234))
        .build();
    let expected = PackageLocator::builder()
        .ecosystem(Ecosystem::Git)
        .package("github.com/foo/bar")
        .org_id(1234)
        .build();
    assert_eq!(expected, with_options);

    let without_options = PackageLocator::builder()
        .ecosystem(Ecosystem::Git)
        .package("github.com/foo/bar")
        .maybe_org_id(None::<usize>)
        .build();
    let expected = PackageLocator::builder()
        .ecosystem(Ecosystem::Git)
        .package("github.com/foo/bar")
        .build();
    assert_eq!(expected, without_options);
}

#[test]
fn trait_impls() {
    const_assert!(impls!(PackageLocator: AsRef<PackageLocator>));
    const_assert!(impls!(PackageLocator: FromStr));
    const_assert!(impls!(PackageLocator: From<StrictLocator>));
    const_assert!(impls!(PackageLocator: From<Locator>));
}

#[test]
fn parse_using_fromstr() {
    let input = "git+github.com/foo/bar";
    let parsed = input.parse().expect("must parse locator");
    let expected = package!(Git, "github.com/foo/bar");
    assert_eq!(expected, parsed);
    assert_eq!(&parsed.to_string(), input);
}

#[test]
fn parse_render_successful() {
    let input = "git+github.com/foo/bar";
    let parsed = PackageLocator::parse(input).expect("must parse locator");
    let expected = PackageLocator::builder()
        .ecosystem(Ecosystem::Git)
        .package("github.com/foo/bar")
        .build();
    assert_eq!(expected, parsed);
    assert_eq!(&parsed.to_string(), input);
}

#[test]
fn parse_drops_revision() {
    let input = "git+github.com/foo/bar$abcd";
    let parsed = PackageLocator::parse(input).expect("must parse locator");
    let expected = PackageLocator::builder()
        .ecosystem(Ecosystem::Git)
        .package("github.com/foo/bar")
        .build();
    assert_eq!(expected, parsed);
}

#[test]
fn parse_invalid_ecosystem() {
    let input = "foo+github.com/foo/bar";
    let parsed = PackageLocator::parse(input);
    assert_matches!(parsed, Err(Error::Parse(ParseError::Ecosystem { .. })));
}

#[test]
fn parse_missing_package() {
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
        let Ok(parsed) = PackageLocator::parse(&input) else {
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
    }
}

#[test]
fn render_with_org() {
    let locator = PackageLocator::builder()
        .ecosystem(Ecosystem::Custom)
        .org_id(1234)
        .package("foo/bar")
        .build();

    let rendered = locator.to_string();
    assert_eq!("custom+1234/foo/bar", rendered);
}

#[test]
fn roundtrip_serialization() {
    let input = PackageLocator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
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
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .build();

    let expected = Test { locator };
    let deserialized = serde_json::from_str(input).expect("must deserialize");
    assert_eq!(expected, deserialized, "deserialize {input}");
}

#[test]
fn promotes_locator() {
    let input = PackageLocator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .build();

    let expected = Locator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .build();
    let promoted = input.clone().promote(None);
    assert_eq!(expected, promoted, "promote {input}");

    let expected = Locator::builder()
        .ecosystem(Ecosystem::Custom)
        .package("foo")
        .org_id(1)
        .revision("bar")
        .build();
    let promoted = input.clone().promote(Some(String::from("bar")));
    assert_eq!(expected, promoted, "promote {input}");
}

#[test]
fn promotes_strict() {
    let input = PackageLocator::builder()
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
        "custom+baz",
        "custom+1/bam",
        "custom+2/bam",
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
