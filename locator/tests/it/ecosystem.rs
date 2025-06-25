use std::collections::HashSet;

use locator::{Ecosystem, EcosystemPrivate, EcosystemPublic, Error, ParseError};
use maplit::hashset;
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
    pretty_assertions::assert_eq!(&ecosystem.to_string(), target, "to_string {ecosystem:?}");
    pretty_assertions::assert_eq!(ecosystem.as_str(), target, "as_str {ecosystem:?}");
    pretty_assertions::assert_eq!(ecosystem.as_ref(), target, "as_ref {ecosystem:?}");

    let serialized = serde_plain::to_string(&ecosystem).expect(&format!("serialize {ecosystem:?}"));
    pretty_assertions::assert_eq!(serialized, target, "serialized {ecosystem:?}");
}

#[test_case(EcosystemPrivate::Archive, "archive"; "archive")]
#[test_case(EcosystemPrivate::CodeSentry, "csbinary"; "code_sentry")]
#[test_case(EcosystemPrivate::Custom, "custom"; "custom")]
#[test_case(EcosystemPrivate::Rpm, "rpm"; "rpm")]
#[test_case(EcosystemPrivate::UnresolvedPath, "upath"; "unresolved_path")]
#[test_case(EcosystemPrivate::User, "user"; "user")]
#[test]
fn render_private(ecosystem: EcosystemPrivate, target: &str) {
    pretty_assertions::assert_eq!(&ecosystem.to_string(), target, "to_string {ecosystem:?}");
    pretty_assertions::assert_eq!(ecosystem.as_str(), target, "as_str {ecosystem:?}");
    pretty_assertions::assert_eq!(ecosystem.as_ref(), target, "as_ref {ecosystem:?}");

    let serialized = serde_plain::to_string(&ecosystem).expect(&format!("serialize {ecosystem:?}"));
    pretty_assertions::assert_eq!(serialized, target, "serialized {ecosystem:?}");
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
    pretty_assertions::assert_eq!(&ecosystem.to_string(), target, "to_string {ecosystem:?}");
    pretty_assertions::assert_eq!(ecosystem.as_str(), target, "as_str {ecosystem:?}");
    pretty_assertions::assert_eq!(ecosystem.as_ref(), target, "as_ref {ecosystem:?}");

    let serialized = serde_plain::to_string(&ecosystem).expect(&format!("serialize {ecosystem:?}"));
    pretty_assertions::assert_eq!(serialized, target, "serialized {ecosystem:?}");
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
    let parsed = serde_plain::from_str::<EcosystemPublic>(target).expect("deserialize");
    pretty_assertions::assert_eq!(parsed, ecosystem, "deserialize {ecosystem:?}");

    let parsed = EcosystemPublic::parse(target).expect("parse");
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
    let parsed = serde_plain::from_str::<Ecosystem>(target).expect("deserialize");
    pretty_assertions::assert_eq!(parsed, ecosystem, "deserialize {ecosystem:?}");

    let parsed = Ecosystem::parse(target).expect("parse");
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
    let parsed = serde_plain::from_str::<EcosystemPrivate>(target).expect("deserialize");
    pretty_assertions::assert_eq!(parsed, ecosystem, "deserialize {ecosystem:?}");

    let parsed = EcosystemPrivate::parse(target).expect("parse");
    pretty_assertions::assert_eq!(parsed, ecosystem, "parse {ecosystem:?}");
}

#[test]
fn parse_fail_all() {
    let parsed = Ecosystem::parse("_does_not_exist").expect_err("parse");
    let expected = hashset! {
        String::from("archive"),
        String::from("bower"),
        String::from("cart"),
        String::from("cargo"),
        String::from("csbinary"),
        String::from("comp"),
        String::from("conan"),
        String::from("conda"),
        String::from("cpan"),
        String::from("cran"),
        String::from("custom"),
        String::from("gem"),
        String::from("git"),
        String::from("go"),
        String::from("hackage"),
        String::from("hex"),
        String::from("apk"),
        String::from("deb"),
        String::from("rpm-generic"),
        String::from("mvn"),
        String::from("npm"),
        String::from("nuget"),
        String::from("pip"),
        String::from("pod"),
        String::from("pub"),
        String::from("swift"),
        String::from("rpm"),
        String::from("upath"),
        String::from("url"),
        String::from("user"),
    };
    let options = match parsed {
        Error::Parse(ParseError::LiteralOneOf { options, .. }) => {
            options.into_iter().collect::<HashSet<_>>()
        }
        _ => panic!("expected `Error::Parse(ParseError::LiteralOneOf {{ .. }})`, got: {parsed:?}"),
    };
    pretty_assertions::assert_eq!(options, expected);
}

#[test]
fn parse_fail_private() {
    let parsed = EcosystemPrivate::parse("_does_not_exist").expect_err("parse");
    let expected = hashset! {
        String::from("archive"),
        String::from("csbinary"),
        String::from("custom"),
        String::from("rpm"),
        String::from("upath"),
        String::from("user"),
    };
    let options = match parsed {
        Error::Parse(ParseError::LiteralOneOf { options, .. }) => {
            options.into_iter().collect::<HashSet<_>>()
        }
        _ => panic!("expected `Error::Parse(ParseError::LiteralOneOf {{ .. }})`, got: {parsed:?}"),
    };
    pretty_assertions::assert_eq!(options, expected);
}

#[test]
fn parse_fail_public() {
    let parsed = EcosystemPublic::parse("_does_not_exist").expect_err("parse");
    let expected = hashset! {
        String::from("bower"),
        String::from("cart"),
        String::from("cargo"),
        String::from("comp"),
        String::from("conan"),
        String::from("conda"),
        String::from("cpan"),
        String::from("cran"),
        String::from("gem"),
        String::from("git"),
        String::from("go"),
        String::from("hackage"),
        String::from("hex"),
        String::from("apk"),
        String::from("deb"),
        String::from("rpm-generic"),
        String::from("mvn"),
        String::from("npm"),
        String::from("nuget"),
        String::from("pip"),
        String::from("pod"),
        String::from("pub"),
        String::from("swift"),
        String::from("url"),
    };
    let options = match parsed {
        Error::Parse(ParseError::LiteralOneOf { options, .. }) => {
            options.into_iter().collect::<HashSet<_>>()
        }
        _ => panic!("expected `Error::Parse(ParseError::LiteralOneOf {{ .. }})`, got: {parsed:?}"),
    };
    pretty_assertions::assert_eq!(options, expected);
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

// Can't use the nice `test_case` macro here since these are all distinct types;
// the following is basically a more verbose recreation of it.
pub mod struct_variants {
    use locator::{Error, ParseError, ecosystems};

    #[duplicate::duplicate_item(
        _name _rendered _test_name;
        [ ecosystems::Archive ] [ "archive" ] [ render_archive ];
        [ ecosystems::Bower ] [ "bower" ] [ render_bower ];
        [ ecosystems::Cart ] [ "cart" ] [ render_cart ];
        [ ecosystems::Cargo ] [ "cargo" ] [ render_cargo ];
        [ ecosystems::CodeSentry ] [ "csbinary" ] [ render_code_sentry ];
        [ ecosystems::Comp ] [ "comp" ] [ render_comp ];
        [ ecosystems::Conan ] [ "conan" ] [ render_conan ];
        [ ecosystems::Conda ] [ "conda" ] [ render_conda ];
        [ ecosystems::Cpan ] [ "cpan" ] [ render_cpan ];
        [ ecosystems::Cran ] [ "cran" ] [ render_cran ];
        [ ecosystems::Custom ] [ "custom" ] [ render_custom ];
        [ ecosystems::Gem ] [ "gem" ] [ render_gem ];
        [ ecosystems::Git ] [ "git" ] [ render_git ];
        [ ecosystems::Go ] [ "go" ] [ render_go ];
        [ ecosystems::Hackage ] [ "hackage" ] [ render_hackage ];
        [ ecosystems::Hex ] [ "hex" ] [ render_hex ];
        [ ecosystems::LinuxAlpine ] [ "apk" ] [ render_linux_alpine ];
        [ ecosystems::LinuxDebian ] [ "deb" ] [ render_linux_debian ];
        [ ecosystems::LinuxRpm ] [ "rpm-generic" ] [ render_linux_rpm ];
        [ ecosystems::Maven ] [ "mvn" ] [ render_maven ];
        [ ecosystems::Npm ] [ "npm" ] [ render_npm ];
        [ ecosystems::Nuget ] [ "nuget" ] [ render_nuget ];
        [ ecosystems::Pip ] [ "pip" ] [ render_pip ];
        [ ecosystems::Pod ] [ "pod" ] [ render_pod ];
        [ ecosystems::Pub ] [ "pub" ] [ render_dart ];
        [ ecosystems::Swift ] [ "swift" ] [ render_swift ];
        [ ecosystems::Rpm ] [ "rpm" ] [ render_rpm ];
        [ ecosystems::UnresolvedPath ] [ "upath" ] [ render_unresolved_path ];
        [ ecosystems::Url ] [ "url" ] [ render_url ];
        [ ecosystems::User ] [ "user" ] [ render_user ];
    )]
    #[test]
    fn _test_name() {
        let name = _name;
        let rendered = _rendered;
        pretty_assertions::assert_eq!(&name.to_string(), rendered, "to_string {name:?}");
        pretty_assertions::assert_eq!(name.as_str(), rendered, "as_str {name:?}");
        pretty_assertions::assert_eq!(name.as_ref(), rendered, "as_ref {name:?}");

        let serialized = serde_plain::to_string(&name).expect(&format!("serialize {name:?}"));
        pretty_assertions::assert_eq!(serialized, rendered, "serialized {name:?}");
    }

    #[duplicate::duplicate_item(
        _name _rendered _test_name;
        [ ecosystems::Archive ] [ "archive" ] [ parse_archive ];
        [ ecosystems::Bower ] [ "bower" ] [ parse_bower ];
        [ ecosystems::Cart ] [ "cart" ] [ parse_cart ];
        [ ecosystems::Cargo ] [ "cargo" ] [ parse_cargo ];
        [ ecosystems::CodeSentry ] [ "csbinary" ] [ parse_code_sentry ];
        [ ecosystems::Comp ] [ "comp" ] [ parse_comp ];
        [ ecosystems::Conan ] [ "conan" ] [ parse_conan ];
        [ ecosystems::Conda ] [ "conda" ] [ parse_conda ];
        [ ecosystems::Cpan ] [ "cpan" ] [ parse_cpan ];
        [ ecosystems::Cran ] [ "cran" ] [ parse_cran ];
        [ ecosystems::Custom ] [ "custom" ] [ parse_custom ];
        [ ecosystems::Gem ] [ "gem" ] [ parse_gem ];
        [ ecosystems::Git ] [ "git" ] [ parse_git ];
        [ ecosystems::Go ] [ "go" ] [ parse_go ];
        [ ecosystems::Hackage ] [ "hackage" ] [ parse_hackage ];
        [ ecosystems::Hex ] [ "hex" ] [ parse_hex ];
        [ ecosystems::LinuxAlpine ] [ "apk" ] [ parse_linux_alpine ];
        [ ecosystems::LinuxDebian ] [ "deb" ] [ parse_linux_debian ];
        [ ecosystems::LinuxRpm ] [ "rpm-generic" ] [ parse_linux_rpm ];
        [ ecosystems::Maven ] [ "mvn" ] [ parse_maven ];
        [ ecosystems::Npm ] [ "npm" ] [ parse_npm ];
        [ ecosystems::Nuget ] [ "nuget" ] [ parse_nuget ];
        [ ecosystems::Pip ] [ "pip" ] [ parse_pip ];
        [ ecosystems::Pod ] [ "pod" ] [ parse_pod ];
        [ ecosystems::Pub ] [ "pub" ] [ parse_dart ];
        [ ecosystems::Swift ] [ "swift" ] [ parse_swift ];
        [ ecosystems::Rpm ] [ "rpm" ] [ parse_rpm ];
        [ ecosystems::UnresolvedPath ] [ "upath" ] [ parse_unresolved_path ];
        [ ecosystems::Url ] [ "url" ] [ parse_url ];
        [ ecosystems::User ] [ "user" ] [ parse_user ];
    )]
    #[test]
    fn _test_name() {
        let input = _rendered;
        _name::parse(input).expect(&format!("parse {input:?}"));
        _name::try_from(input).expect(&format!("try_from {input:?}"));
        _name::try_from(String::from(input)).expect(&format!("try_from_string {input:?}"));
        _name::try_from(&String::from(input)).expect(&format!("try_from_refstring {input:?}"));
        serde_plain::from_str::<_name>(input).expect(&format!("deserialize {input:?}"));

        let parsed = _name::parse("_does_not_exist").expect_err(&format!("parse nonexistent"));
        let expected = match parsed {
            Error::Parse(ParseError::LiteralExact { expected, .. }) => expected,
            _ => panic!(
                "expected `Error::Parse(ParseError::LiteralExact {{ .. }})`, got: {parsed:?}"
            ),
        };
        pretty_assertions::assert_eq!(expected, input);
    }
}
