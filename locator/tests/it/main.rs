//! Tests for the crate.

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
