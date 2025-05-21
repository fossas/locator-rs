use std::{fmt::Display, str::FromStr};

use bon::Builder;
use documented::Documented;
use getset::{CopyGetters, Getters};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::borrow::Cow;
use utoipa::{
    PartialSchema, ToSchema,
    openapi::{ObjectBuilder, Type},
};

use crate::{
    Error, OrgId, Package, PackageLocator, ParseError, Protocol, Revision, StrictLocator,
    parse_org_package,
};

/// Convenience macro for creating a [`Locator`].
/// Required types and fields are checked at compile time.
///
/// ```
/// let loc = locator::locator!(Npm, "lodash");
/// assert_eq!("npm+lodash", &loc.to_string());
///
/// let loc = locator::locator!(Npm, "lodash", "1.0.0");
/// assert_eq!("npm+lodash$1.0.0", &loc.to_string());
///
/// let loc = locator::locator!(org 1234 => Npm, "lodash");
/// assert_eq!("npm+1234/lodash", &loc.to_string());
///
/// let loc = locator::locator!(org 1234 => Npm, "lodash", "1.0.0");
/// assert_eq!("npm+1234/lodash$1.0.0", &loc.to_string());
/// ```
#[macro_export]
macro_rules! locator {
    (org $org:expr => $protocol:ident, $package:expr, $version:expr) => {
        $crate::Locator::builder()
            .protocol($crate::Protocol::$protocol)
            .package($package)
            .org_id($org)
            .revision($version)
            .build()
    };
    (org $org:expr => $protocol:ident, $package:expr) => {
        $crate::Locator::builder()
            .protocol($crate::Protocol::$protocol)
            .package($package)
            .org_id($org)
            .build()
    };
    ($protocol:ident, $package:expr, $version:expr) => {
        $crate::Locator::builder()
            .protocol($crate::Protocol::$protocol)
            .package($package)
            .revision($version)
            .build()
    };
    ($protocol:ident, $package:expr) => {
        $crate::Locator::builder()
            .protocol($crate::Protocol::$protocol)
            .package($package)
            .build()
    };
}

/// The regular expression used to parse the locator.
///
/// ```
/// # use locator::locator_regex;
///
/// // Get the raw string used for the expression.
/// let expression = locator_regex!();
///
/// // Parse the regular expression.
/// // The expression is compiled once per callsite.
/// let parsed = locator_regex!(parse => "git+github.com/fossas/locator-rs$v2.2.0");
/// ```
#[macro_export]
#[doc(hidden)]
macro_rules! locator_regex {
    () => {
        r"^(?:([a-z-]+)\+|)([^$]+)(?:\$|)(.+|)$"
    };
    (parse => $input:expr) => {
        lazy_regex::regex_captures!(r"^(?:([a-z-]+)\+|)([^$]+)(?:\$|)(.+|)$", $input)
    };
}

/// `Locator` identifies a package, optionally at a specific revision, in a code host.
///
/// If the `revision` component is not specified, FOSSA services interpret this to mean
/// that the "latest" version of the package should be used if the requested operation
/// requires a concrete version of the package.
///
/// ## Guarantees
///
/// This type represents a _validly-constructed_ `Locator`, but does not
/// guarantee whether a package or revision actually exists or is accessible
/// in the code host.
///
/// ## Ordering
///
/// `Locator` orders by:
/// 1. protocol, alphanumerically.
/// 2. Organization ID, alphanumerically; missing organizations are sorted higher.
/// 3. The package field, alphanumerically.
/// 4. The revision field:
///   - If both comparing locators use semver, these are compared using semver rules.
///   - Otherwise these are compared alphanumerically.
///   - Missing revisions are sorted higher.
///
/// **Important:** there may be other metrics for ordering using the actual code host
/// which contains the package- for example ordering by release date, or code hosts
/// such as `git` which have non-linear history (making flat ordering a lossy operation).
/// `Locator` does not take such edge cases into account in any way.
///
/// ## Parsing
///
/// This type is canonically rendered to a string before being serialized
/// to the database or sent over the network according to the rules in this section.
///
/// The input string must be in one of the following formats:
/// ```ignore
/// {protocol}+{package}${revision}
/// {protocol}+{package}
/// ```
///
/// Packages may also be namespaced to a specific organization;
/// in such cases the organization ID is at the start of the `{package}` field
/// separated by a slash. The ID can be any non-negative integer.
/// This yields the following optional formats:
/// ```ignore
/// {protocol}+{org_id}/{package}${revision}
/// {protocol}+{org_id}/{package}
/// ```
///
/// Note that locators do not feature escaping: instead the _first_ instance
/// of each delimiter (`+`, `/`, `$`) is used to split the fields. However,
/// as a special case organization IDs are only extracted if the field content
/// fully consists of a non-negative integer.
//
// For more information on the background of `Locator` and protocols generally,
// FOSSA employees may refer to the "protocols and locators" doc: https://go/protocols-doc.
#[derive(
    Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Builder, Getters, CopyGetters, Documented,
)]
pub struct Locator {
    /// Determines which protocol is used to download this package.
    #[getset(get_copy = "pub")]
    protocol: Protocol,

    /// Specifies the organization ID to which this package is namespaced.
    ///
    /// Locators are namespaced to an organization when FOSSA needs to use the
    /// private repositories or settings configured by the user to resolve the package.
    ///
    /// Generally, users can treat this as an implementation detail:
    /// Organization IDs namespacing a package means the package should concretely be considered different;
    /// for example `npm+lodash$1.0.0` should be considered different from `npm+1234/lodash$1.0.0`.
    /// The reasoning for this is that private packages may be totally different than
    /// a similarly named public package- in the example above, both of them being `lodash@1.0.0`
    /// doesn't really imply that they are both the popular project known as "lodash".
    /// We know the public one is (`npm+lodash$1.0.0`), but the private one could be anything.
    ///
    /// Examples:
    /// - A public Maven package that is hosted on Maven Central is not namespaced.
    /// - A private Maven package that is hosted on a private host is namespaced.
    /// - A public NPM package that is hosted on NPM is not namespaced.
    /// - A private NPM package that is hosted on NPM but requires credentials is namespaced.
    #[builder(into)]
    #[getset(get_copy = "pub")]
    org_id: Option<OrgId>,

    /// Specifies the unique identifier for the package by protocol.
    ///
    /// For example, the `git` protocol fetching a github package
    /// uses a value in the form of `{user_name}/{package_name}`.
    #[builder(into)]
    #[getset(get = "pub")]
    package: Package,

    /// Specifies the version for the package by protocol.
    ///
    /// For example, the `git` protocol fetching a github package
    /// uses a value in the form of `{git_sha}` or `{git_tag}`,
    /// and the protocol disambiguates.
    #[builder(into)]
    #[getset(get = "pub")]
    revision: Option<Revision>,
}

impl Locator {
    /// The regular expression used to parse locators.
    pub const REGEX: &'static str = locator_regex!();

    /// Parse a `Locator`.
    /// For details, see the parsing section on [`Locator`].
    pub fn parse(input: &str) -> Result<Self, Error> {
        /// Convenience macro for fatal errors without needing to type out all the `.into()`s.
        macro_rules! fatal {
            ($type:ident => $input:expr) => {
                ParseError::$type {
                    input: $input.into(),
                }
            };
            ($type:ident => $input:expr, $($key:ident: $value:expr),+) => {
                ParseError::$type {
                    input: $input.into(),
                    $($key: $value.into()),*,
                }
            };
        }

        /// Convenience macro for early returns.
        macro_rules! bail {
            ($($tt:tt)*) => {
                return Err(Error::from(fatal!($($tt)*)))
            };
        }

        let Some((_, protocol, package, revision)) = locator_regex!(parse => input) else {
            bail!(Syntax => input);
        };

        if protocol.is_empty() {
            bail!(Field => input, field: "protocol");
        }
        if package.is_empty() {
            bail!(Field => input, field: "package");
        }

        let protocol = Protocol::try_from(protocol)
            .map_err(|err| fatal!(Protocol => input, protocol: protocol, error: err))?;

        let revision = if revision.is_empty() {
            None
        } else {
            Some(Revision::from(revision))
        };

        let (org_id, package) = parse_org_package(package);
        Ok(Locator {
            protocol,
            org_id,
            package,
            revision,
        })
    }

    /// Promote a `Locator` to a [`StrictLocator`] if it has a `revision` component;
    /// if not this function returns `Err` with the original locator.
    pub fn try_promote_strict(self) -> Result<StrictLocator, Self> {
        let locator = match self.revision {
            None => return Err(self),
            Some(rev) => StrictLocator::builder()
                .protocol(self.protocol)
                .package(self.package)
                .revision(rev),
        };

        Ok(match self.org_id {
            None => locator.build(),
            Some(OrgId(id)) => locator.org_id(id).build(),
        })
    }

    /// Promote a `Locator` to a [`StrictLocator`] by providing the default value to use
    /// for the `revision` component, if one is not specified in the locator already.
    ///
    /// The `ToString` implementation is lazily evaluated if the locator doesn't already contain a revision.
    pub fn promote_strict(self, revision: impl ToString) -> StrictLocator {
        let locator = StrictLocator::builder()
            .protocol(self.protocol)
            .package(self.package)
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
            .protocol(self.protocol)
            .package(self.package)
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
    pub(crate) fn explode(self) -> (Protocol, Option<OrgId>, Package, Option<Revision>) {
        (self.protocol, self.org_id, self.package, self.revision)
    }
}

impl Display for Locator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let protocol = &self.protocol;
        write!(f, "{protocol}+")?;

        let package = &self.package;
        if let Some(org_id) = &self.org_id {
            write!(f, "{org_id}/")?;
        }
        write!(f, "{package}")?;

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

impl From<PackageLocator> for Locator {
    fn from(package: PackageLocator) -> Self {
        let (protocol, org_id, package) = package.explode();
        Self {
            protocol,
            org_id,
            package,
            revision: None,
        }
    }
}

impl From<&PackageLocator> for Locator {
    fn from(package: &PackageLocator) -> Self {
        Self {
            protocol: package.protocol(),
            org_id: package.org_id(),
            package: package.package().clone(),
            revision: None,
        }
    }
}

impl From<StrictLocator> for Locator {
    fn from(strict: StrictLocator) -> Self {
        let (protocol, org_id, package, revision) = strict.explode();
        Self {
            protocol,
            org_id,
            package,
            revision: Some(revision),
        }
    }
}

impl From<&StrictLocator> for Locator {
    fn from(strict: &StrictLocator) -> Self {
        Self {
            protocol: strict.protocol(),
            org_id: strict.org_id(),
            package: strict.package().clone(),
            revision: Some(strict.revision().clone()),
        }
    }
}

impl From<&Locator> for Locator {
    fn from(locator: &Locator) -> Self {
        locator.clone()
    }
}

impl AsRef<Locator> for Locator {
    fn as_ref(&self) -> &Locator {
        self
    }
}

impl FromStr for Locator {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s)
    }
}

impl PartialSchema for Locator {
    fn schema() -> utoipa::openapi::RefOr<utoipa::openapi::schema::Schema> {
        ObjectBuilder::new()
            .description(Some(Self::DOCS))
            .examples([json!("git+github.com/fossas/locator-rs$v1.0.0")])
            .min_length(Some(3))
            .schema_type(Type::String)
            .build()
            .into()
    }
}

impl ToSchema for Locator {
    fn name() -> Cow<'static, str> {
        Cow::Borrowed("Locator")
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use impls::impls;
    use itertools::{Itertools, izip};
    use pretty_assertions::assert_eq;
    use proptest::prelude::*;
    use serde::Deserialize;
    use static_assertions::const_assert;

    use super::*;

    #[test]
    fn from_existing() {
        let first = locator!(Git, "github.com/foo/bar");
        let second = Locator::builder()
            .protocol(first.protocol())
            .maybe_org_id(first.org_id())
            .package(first.package())
            .maybe_revision(first.revision().as_ref())
            .build();
        assert_eq!(first, second);
    }

    #[test]
    fn optional_fields() {
        let with_options = Locator::builder()
            .protocol(Protocol::Git)
            .package("github.com/foo/bar")
            .maybe_org_id(Some(1234))
            .maybe_revision(Some("abcd"))
            .build();
        let expected = Locator::builder()
            .protocol(Protocol::Git)
            .package("github.com/foo/bar")
            .org_id(1234)
            .revision("abcd")
            .build();
        assert_eq!(expected, with_options);

        let without_options = Locator::builder()
            .protocol(Protocol::Git)
            .package("github.com/foo/bar")
            .maybe_org_id(None::<usize>)
            .maybe_revision(None::<&str>)
            .build();
        let expected = Locator::builder()
            .protocol(Protocol::Git)
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
            .protocol(Protocol::Git)
            .package("github.com/foo/bar")
            .build();
        assert_eq!(expected, parsed);
        assert_eq!(&parsed.to_string(), input);

        let input = "git+github.com/foo/bar$abcd";
        let parsed = Locator::parse(input).expect("must parse locator");
        let expected = Locator::builder()
            .protocol(Protocol::Git)
            .package("github.com/foo/bar")
            .revision("abcd")
            .build();
        assert_eq!(expected, parsed);
        assert_eq!(&parsed.to_string(), input);
    }

    #[test]
    fn parse_invalid_protocol() {
        let input = "foo+github.com/foo/bar";
        let parsed = Locator::parse(input);
        assert_matches!(parsed, Err(Error::Parse(ParseError::Protocol { .. })));
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
        let protocols = Protocol::iter().map(|protocol| format!("{protocol}"));
        let orgs = [
            OrgId(0usize),
            OrgId(1),
            OrgId(1234),
            OrgId(2385028),
            OrgId(19847938492847928),
        ];
        let packages = ["github.com/foo/bar", "some-name"];
        let revisions = ["", "$", "$1", "$1234abcd1234"];

        for (protocol, org, package, revision) in izip!(protocols, orgs, packages, revisions) {
            let input = format!("{protocol}+{org}/{package}{revision}");
            let Ok(parsed) = Locator::parse(&input) else {
                panic!("must parse '{input}'")
            };

            assert_eq!(
                parsed.protocol().to_string(),
                protocol,
                "'protocol' in '{input}' must match"
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
            .protocol(Protocol::Custom)
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
            .protocol(Protocol::Custom)
            .package("foo/bar")
            .revision("123abc")
            .build();

        let rendered = locator.to_string();
        assert_eq!("custom+foo/bar$123abc", rendered);
    }

    #[test]
    fn render_package() {
        let locator = Locator::builder()
            .protocol(Protocol::Custom)
            .package("foo/bar")
            .build();

        let rendered = locator.to_string();
        assert_eq!("custom+foo/bar", rendered);
    }

    #[test]
    fn roundtrip_serialization() {
        let input = Locator::builder()
            .protocol(Protocol::Custom)
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
            .protocol(Protocol::Custom)
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
            .protocol(Protocol::Custom)
            .package("foo")
            .org_id(1)
            .revision("abcd")
            .build();

        let expected = PackageLocator::builder()
            .protocol(Protocol::Custom)
            .package("foo")
            .org_id(1)
            .build();
        let demoted = input.clone().into_package();
        assert_eq!(expected, demoted, "demote {input}");
    }

    #[test]
    fn promotes_strict() {
        let input = Locator::builder()
            .protocol(Protocol::Custom)
            .package("foo")
            .org_id(1)
            .build();

        let expected = StrictLocator::builder()
            .protocol(Protocol::Custom)
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
            .protocol(Protocol::Custom)
            .package("foo")
            .revision("1234")
            .org_id(1)
            .build();

        let expected = StrictLocator::builder()
            .protocol(Protocol::Custom)
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
            .protocol(Protocol::Custom)
            .package("foo")
            .org_id(1)
            .build();

        let expected = StrictLocator::builder()
            .protocol(Protocol::Custom)
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
            .protocol(Protocol::Custom)
            .package("foo")
            .revision("1234")
            .org_id(1)
            .build();

        let expected = StrictLocator::builder()
            .protocol(Protocol::Custom)
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
            .protocol(Protocol::Custom)
            .package("foo")
            .revision("1234")
            .org_id(1)
            .build();

        let expected = StrictLocator::builder()
            .protocol(Protocol::Custom)
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
            .protocol(Protocol::Custom)
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
}
