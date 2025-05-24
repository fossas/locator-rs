use locator::{Ecosystem, EcosystemPrivate, EcosystemPublic};
use simple_test_case::test_case;

#[test_case(Ecosystem::Archive, "archive"; "archive")]
#[test_case(Ecosystem::Bower, "bower"; "bower")]
#[test_case(Ecosystem::Cart, "cart"; "cart")]
#[test_case(Ecosystem::Cargo, "cargo"; "cargo")]
#[test_case(Ecosystem::CodeSentry, "csbinary"; "code_sentry")]
#[test_case(Ecosystem::Comp, "comp"; "comp")]
#[test_case(Ecosystem::Conan, "conan"; "conan")]
#[test_case(Ecosystem::Conda, "conda"; "conda")]
#[test_case(Ecosystem::Cpan, "cpan"; "cpan")]
#[test_case(Ecosystem::Cran, "cran"; "cran")]
#[test_case(Ecosystem::Custom, "custom"; "custom")]
#[test_case(Ecosystem::Gem, "gem"; "gem")]
#[test_case(Ecosystem::Git, "git"; "git")]
#[test_case(Ecosystem::Go, "go"; "go")]
#[test_case(Ecosystem::Hackage, "hackage"; "hackage")]
#[test_case(Ecosystem::Hex, "hex"; "hex")]
#[test_case(Ecosystem::LinuxAlpine, "apk"; "linux_alpine")]
#[test_case(Ecosystem::LinuxDebian, "deb"; "linux_debian")]
#[test_case(Ecosystem::LinuxRpm, "rpm-generic"; "linux_rpm")]
#[test_case(Ecosystem::Maven, "mvn"; "maven")]
#[test_case(Ecosystem::Npm, "npm"; "npm")]
#[test_case(Ecosystem::Nuget, "nuget"; "nuget")]
#[test_case(Ecosystem::Pip, "pip"; "pip")]
#[test_case(Ecosystem::Pod, "pod"; "pod")]
#[test_case(Ecosystem::Pub, "pub"; "dart")]
#[test_case(Ecosystem::Swift, "swift"; "swift")]
#[test_case(Ecosystem::Rpm, "rpm"; "rpm")]
#[test_case(Ecosystem::UnresolvedPath, "upath"; "unresolved_path")]
#[test_case(Ecosystem::Url, "url"; "url")]
#[test_case(Ecosystem::User, "user"; "user")]
#[test]
fn render_all(ecosystem: Ecosystem, target: &str) {
    pretty_assertions::assert_eq!(&ecosystem.to_string(), target, "render {ecosystem:?}");
}

#[test_case(EcosystemPrivate::Archive, "archive"; "archive")]
#[test_case(EcosystemPrivate::CodeSentry, "csbinary"; "code_sentry")]
#[test_case(EcosystemPrivate::Custom, "custom"; "custom")]
#[test_case(EcosystemPrivate::Rpm, "rpm"; "rpm")]
#[test_case(EcosystemPrivate::UnresolvedPath, "upath"; "unresolved_path")]
#[test_case(EcosystemPrivate::User, "user"; "user")]
#[test]
fn render_private(ecosystem: EcosystemPrivate, target: &str) {
    pretty_assertions::assert_eq!(&ecosystem.to_string(), target, "render {ecosystem:?}");
}

#[test_case(EcosystemPublic::Bower, "bower"; "bower")]
#[test_case(EcosystemPublic::Cart, "cart"; "cart")]
#[test_case(EcosystemPublic::Cargo, "cargo"; "cargo")]
#[test_case(EcosystemPublic::Comp, "comp"; "comp")]
#[test_case(EcosystemPublic::Conan, "conan"; "conan")]
#[test_case(EcosystemPublic::Conda, "conda"; "conda")]
#[test_case(EcosystemPublic::Cpan, "cpan"; "cpan")]
#[test_case(EcosystemPublic::Cran, "cran"; "cran")]
#[test_case(EcosystemPublic::Gem, "gem"; "gem")]
#[test_case(EcosystemPublic::Git, "git"; "git")]
#[test_case(EcosystemPublic::Go, "go"; "go")]
#[test_case(EcosystemPublic::Hackage, "hackage"; "hackage")]
#[test_case(EcosystemPublic::Hex, "hex"; "hex")]
#[test_case(EcosystemPublic::LinuxAlpine, "apk"; "linux_alpine")]
#[test_case(EcosystemPublic::LinuxDebian, "deb"; "linux_debian")]
#[test_case(EcosystemPublic::LinuxRpm, "rpm-generic"; "linux_rpm")]
#[test_case(EcosystemPublic::Maven, "mvn"; "maven")]
#[test_case(EcosystemPublic::Npm, "npm"; "npm")]
#[test_case(EcosystemPublic::Nuget, "nuget"; "nuget")]
#[test_case(EcosystemPublic::Pip, "pip"; "pip")]
#[test_case(EcosystemPublic::Pod, "pod"; "pod")]
#[test_case(EcosystemPublic::Pub, "pub"; "dart")]
#[test_case(EcosystemPublic::Swift, "swift"; "swift")]
#[test_case(EcosystemPublic::Url, "url"; "url")]
#[test]
fn render_public(ecosystem: EcosystemPublic, target: &str) {
    pretty_assertions::assert_eq!(&ecosystem.to_string(), target, "render {ecosystem:?}");
}

#[test_case(EcosystemPublic::Bower, "bower"; "bower")]
#[test_case(EcosystemPublic::Cart, "cart"; "cart")]
#[test_case(EcosystemPublic::Cargo, "cargo"; "cargo")]
#[test_case(EcosystemPublic::Comp, "comp"; "comp")]
#[test_case(EcosystemPublic::Conan, "conan"; "conan")]
#[test_case(EcosystemPublic::Conda, "conda"; "conda")]
#[test_case(EcosystemPublic::Cpan, "cpan"; "cpan")]
#[test_case(EcosystemPublic::Cran, "cran"; "cran")]
#[test_case(EcosystemPublic::Gem, "gem"; "gem")]
#[test_case(EcosystemPublic::Git, "git"; "git")]
#[test_case(EcosystemPublic::Go, "go"; "go")]
#[test_case(EcosystemPublic::Hackage, "hackage"; "hackage")]
#[test_case(EcosystemPublic::Hex, "hex"; "hex")]
#[test_case(EcosystemPublic::LinuxAlpine, "apk"; "linux_alpine")]
#[test_case(EcosystemPublic::LinuxDebian, "deb"; "linux_debian")]
#[test_case(EcosystemPublic::LinuxRpm, "rpm-generic"; "linux_rpm")]
#[test_case(EcosystemPublic::Maven, "mvn"; "maven")]
#[test_case(EcosystemPublic::Npm, "npm"; "npm")]
#[test_case(EcosystemPublic::Nuget, "nuget"; "nuget")]
#[test_case(EcosystemPublic::Pip, "pip"; "pip")]
#[test_case(EcosystemPublic::Pod, "pod"; "pod")]
#[test_case(EcosystemPublic::Pub, "pub"; "dart")]
#[test_case(EcosystemPublic::Swift, "swift"; "swift")]
#[test_case(EcosystemPublic::Url, "url"; "url")]
#[test]
fn parse_public(ecosystem: EcosystemPublic, target: &str) {
    let parsed = serde_plain::from_str::<EcosystemPublic>(target).expect("parse ecosystem");
    pretty_assertions::assert_eq!(parsed, ecosystem, "parse {ecosystem:?}");
}

#[test_case(Ecosystem::Archive, "archive"; "archive")]
#[test_case(Ecosystem::Bower, "bower"; "bower")]
#[test_case(Ecosystem::Cart, "cart"; "cart")]
#[test_case(Ecosystem::Cargo, "cargo"; "cargo")]
#[test_case(Ecosystem::CodeSentry, "csbinary"; "code_sentry")]
#[test_case(Ecosystem::Comp, "comp"; "comp")]
#[test_case(Ecosystem::Conan, "conan"; "conan")]
#[test_case(Ecosystem::Conda, "conda"; "conda")]
#[test_case(Ecosystem::Cpan, "cpan"; "cpan")]
#[test_case(Ecosystem::Cran, "cran"; "cran")]
#[test_case(Ecosystem::Custom, "custom"; "custom")]
#[test_case(Ecosystem::Gem, "gem"; "gem")]
#[test_case(Ecosystem::Git, "git"; "git")]
#[test_case(Ecosystem::Go, "go"; "go")]
#[test_case(Ecosystem::Hackage, "hackage"; "hackage")]
#[test_case(Ecosystem::Hex, "hex"; "hex")]
#[test_case(Ecosystem::LinuxAlpine, "apk"; "linux_alpine")]
#[test_case(Ecosystem::LinuxDebian, "deb"; "linux_debian")]
#[test_case(Ecosystem::LinuxRpm, "rpm-generic"; "linux_rpm")]
#[test_case(Ecosystem::Maven, "mvn"; "maven")]
#[test_case(Ecosystem::Npm, "npm"; "npm")]
#[test_case(Ecosystem::Nuget, "nuget"; "nuget")]
#[test_case(Ecosystem::Pip, "pip"; "pip")]
#[test_case(Ecosystem::Pod, "pod"; "pod")]
#[test_case(Ecosystem::Pub, "pub"; "dart")]
#[test_case(Ecosystem::Swift, "swift"; "swift")]
#[test_case(Ecosystem::Rpm, "rpm"; "rpm")]
#[test_case(Ecosystem::UnresolvedPath, "upath"; "unresolved_path")]
#[test_case(Ecosystem::Url, "url"; "url")]
#[test_case(Ecosystem::User, "user"; "user")]
#[test]
fn parse_all(ecosystem: Ecosystem, target: &str) {
    let parsed = serde_plain::from_str::<Ecosystem>(target).expect("parse ecosystem");
    pretty_assertions::assert_eq!(parsed, ecosystem, "parse {ecosystem:?}");
}

#[test_case(EcosystemPrivate::Archive, "archive"; "archive")]
#[test_case(EcosystemPrivate::CodeSentry, "csbinary"; "code_sentry")]
#[test_case(EcosystemPrivate::Custom, "custom"; "custom")]
#[test_case(EcosystemPrivate::Rpm, "rpm"; "rpm")]
#[test_case(EcosystemPrivate::UnresolvedPath, "upath"; "unresolved_path")]
#[test_case(EcosystemPrivate::User, "user"; "user")]
#[test]
fn parse_private(ecosystem: EcosystemPrivate, target: &str) {
    let parsed = serde_plain::from_str::<EcosystemPrivate>(target).expect("parse ecosystem");
    pretty_assertions::assert_eq!(parsed, ecosystem, "parse {ecosystem:?}");
}

#[test]
fn iter_all() {
    let expected = vec![
        Ecosystem::Archive,
        Ecosystem::Bower,
        Ecosystem::Cart,
        Ecosystem::Cargo,
        Ecosystem::CodeSentry,
        Ecosystem::Comp,
        Ecosystem::Conan,
        Ecosystem::Conda,
        Ecosystem::Cpan,
        Ecosystem::Cran,
        Ecosystem::Custom,
        Ecosystem::Gem,
        Ecosystem::Git,
        Ecosystem::Go,
        Ecosystem::Hackage,
        Ecosystem::Hex,
        Ecosystem::LinuxAlpine,
        Ecosystem::LinuxDebian,
        Ecosystem::LinuxRpm,
        Ecosystem::Maven,
        Ecosystem::Npm,
        Ecosystem::Nuget,
        Ecosystem::Pip,
        Ecosystem::Pod,
        Ecosystem::Pub,
        Ecosystem::Swift,
        Ecosystem::Rpm,
        Ecosystem::UnresolvedPath,
        Ecosystem::Url,
        Ecosystem::User,
    ];
    let iterated = Ecosystem::iter().collect::<Vec<_>>();
    pretty_assertions::assert_eq!(iterated, expected);
}

#[test]
fn iter_public() {
    let expected = vec![
        EcosystemPublic::Bower,
        EcosystemPublic::Cart,
        EcosystemPublic::Cargo,
        EcosystemPublic::Comp,
        EcosystemPublic::Conan,
        EcosystemPublic::Conda,
        EcosystemPublic::Cpan,
        EcosystemPublic::Cran,
        EcosystemPublic::Gem,
        EcosystemPublic::Git,
        EcosystemPublic::Go,
        EcosystemPublic::Hackage,
        EcosystemPublic::Hex,
        EcosystemPublic::LinuxAlpine,
        EcosystemPublic::LinuxDebian,
        EcosystemPublic::LinuxRpm,
        EcosystemPublic::Maven,
        EcosystemPublic::Npm,
        EcosystemPublic::Nuget,
        EcosystemPublic::Pip,
        EcosystemPublic::Pod,
        EcosystemPublic::Pub,
        EcosystemPublic::Swift,
        EcosystemPublic::Url,
    ];
    let iterated = EcosystemPublic::iter().collect::<Vec<_>>();
    pretty_assertions::assert_eq!(iterated, expected);
}

#[test]
fn iter_private() {
    let expected = vec![
        EcosystemPrivate::Archive,
        EcosystemPrivate::CodeSentry,
        EcosystemPrivate::Custom,
        EcosystemPrivate::Rpm,
        EcosystemPrivate::UnresolvedPath,
        EcosystemPrivate::User,
    ];
    let iterated = EcosystemPrivate::iter().collect::<Vec<_>>();
    pretty_assertions::assert_eq!(iterated, expected);
}
