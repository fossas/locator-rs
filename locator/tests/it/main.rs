//! Tests for the crate.

use monostate::MustBe;
use simple_test_case::test_case;

use locator::*;

mod constraint;
mod ecosystem;
mod locator_package;
mod locator_strict;
mod locator_t;

#[test_case(r#""rpm-generic""#, Ecosystem::LinuxRpm; "rpm-generic")]
#[test_case(r#""deb""#, Ecosystem::LinuxDebian; "deb")]
#[test_case(r#""apk""#, Ecosystem::LinuxAlpine; "apk")]
#[test]
fn serializes_linux_properly(expected: &str, value: Ecosystem) {
    assert_eq!(expected, serde_json::to_string(&value).unwrap());
}

#[test_case(Package::from("name"); "name")]
#[test_case(Package::from("name/foo"); "name/foo")]
#[test_case(Package::from("/name/foo"); "/name/foo")]
#[test_case(Package::from("/name"); "/name")]
#[test_case(Package::from("abcd/1234/name"); "abcd/1234/name")]
#[test_case(Package::from("1abc2/name"); "1abc2/name")]
#[test]
fn package_roundtrip(package: Package) {
    let serialized = serde_json::to_string(&package).expect("must serialize");
    let deserialized = serde_json::from_str(&serialized).expect("must deserialize");
    assert_eq!(package, deserialized);
}

#[test_case("1.0.0", revision!(parse_semver => "1.0.0"); "1.0.0")]
#[test_case("1.2.0", revision!(parse_semver => "1.2.0"); "1.2.0")]
#[test_case("1.0.0-alpha.1", revision!(parse_semver => "1.0.0-alpha.1"); "1.0.0-alpha.1")]
#[test_case("1.0.0-alpha1", revision!(parse_semver => "1.0.0-alpha1"); "1.0.0-alpha1")]
#[test_case("1.0.0-rc.10+r1234", revision!(parse_semver => "1.0.0-rc.10+r1234"); "1.0.0-rc.10+r1234")]
#[test_case("abcd1234", revision!("abcd1234"); "abcd1234")]
#[test_case("v1.0.0", revision!("v1.0.0"); "v1.0.0")]
#[test]
fn revision(revision: &str, expected: Revision) {
    let serialized = serde_json::to_string(&revision).expect("must serialize");
    let deserialized = serde_json::from_str(&serialized).expect("must deserialize");
    assert_eq!(expected, deserialized);
}

#[test_case(revision!("1.0.0"); "1.0.0")]
#[test_case(revision!("1.2.0"); "1.2.0")]
#[test_case(revision!("1.0.0-alpha.1"); "1.0.0-alpha.1")]
#[test_case(revision!("1.0.0-alpha1"); "1.0.0-alpha1")]
#[test_case(revision!("1.0.0-rc.10"); "1.0.0-rc.10")]
#[test_case(revision!("abcd1234"); "abcd1234")]
#[test_case(revision!("v1.0.0"); "v1.0.0")]
#[test]
fn revision_roundtrip(revision: Revision) {
    let serialized = serde_json::to_string(&revision).expect("must serialize");
    let deserialized = serde_json::from_str(&serialized).expect("must deserialize");
    assert_eq!(revision, deserialized);
}

#[test_case(OrgId::from(1); "1")]
#[test_case(OrgId::from(0); "0")]
#[test_case(OrgId::from(1210931039); "1210931039")]
#[test]
fn org_roundtrip(org: OrgId) {
    let serialized = serde_json::to_string(&org).expect("must serialize");
    let deserialized = serde_json::from_str(&serialized).expect("must deserialize");
    assert_eq!(org, deserialized);
}

/// For this test let's say we don't care what's in a locator, just want to read the fields.
#[test]
fn custom_newtype_flexible() {
    #[locator_parts(types(String, String, String, String))]
    struct FlexibleLocator;

    let source = locator!(org 1234 => Custom, "pkg", "rev").to_string();
    let parsed = FlexibleLocator::parse(&source).expect("parse");
    assert_eq!(parsed.ecosystem(), "custom");
    assert_eq!(parsed.organization(), "1234");
    assert_eq!(parsed.package(), "pkg");
    assert_eq!(parsed.revision(), "rev");

    let source = locator!(Custom, "pkg").to_string();
    let parsed = FlexibleLocator::parse(&source).expect("parse with no revision or org");
    assert_eq!(parsed.ecosystem(), "custom");
    assert_eq!(parsed.organization(), "");
    assert_eq!(parsed.package(), "pkg");
    assert_eq!(parsed.revision(), "");
}

/// For this test let's say we just want to parse the `Package` out of the string.
#[test]
fn custom_newtype_pkgname() {
    #[locator_parts(types(Empty, Empty, String, Empty))]
    struct PkgNameLocator;

    let source = locator!(org 1234 => Custom, "pkg", "rev").to_string();
    let parsed = PkgNameLocator::parse(&source).expect("parse");
    assert_eq!(parsed.package(), "pkg");

    let source = locator!(Custom, "pkg").to_string();
    let parsed = PkgNameLocator::parse(&source).expect("parse with no revision or org");
    assert_eq!(parsed.package(), "pkg");
}

/// For this test let's say we just want to parse the `Package` out of the string.
#[test]
fn custom_newtype_ecosystem_private() {
    #[locator_parts(types(EcosystemPrivate, Option<OrgId>, Package, Option<Revision>))]
    #[derive(Debug)]
    struct PrivateLocator;

    let source = locator!(Custom, "pkg").to_string();
    let parsed = PrivateLocator::parse(&source).expect("can parse a private locator");
    assert_eq!(parsed.package(), &Package::from("pkg"));

    let source = locator!(Npm, "pkg").to_string();
    PrivateLocator::parse(&source).expect_err("only private locators work");
}

/// For this test let's say we only want to parse if the package is a specific value.
#[test]
fn custom_newtype_constrained_package() {
    #[locator_parts(types(Empty, Empty, MustBe!("good"), String))]
    #[derive(Debug)]
    struct GoodLocator;

    let source = locator!(Custom, "good", "1.0").to_string();
    let parsed = GoodLocator::parse(&source).expect("can parse a good locator");
    assert_eq!(parsed.revision(), "1.0");

    let source = locator!(Custom, "bad", "1.0").to_string();
    GoodLocator::parse(&source).expect_err("can't parse a bad locator");
}
