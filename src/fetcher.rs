use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use strum::{AsRefStr, Display, EnumIter, EnumString};

/// [`Locator`](crate::Locator) is closely tied with the concept of Core's "fetchers",
/// which are asynchronous jobs tasked with downloading the code
/// referred to by a [`Locator`](crate::Locator) so that Core or some other service
/// may analyze it.
///
/// For more information on the background of `Locator` and fetchers generally,
/// refer to [Fetchers and Locators](https://go/fetchers-doc).
#[derive(
    Copy,
    Clone,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Debug,
    Display,
    EnumString,
    EnumIter,
    AsRefStr,
    Serialize,
    Deserialize,
    JsonSchema,
)]
#[non_exhaustive]
#[serde(rename_all = "snake_case")]
pub enum Fetcher {
    /// Archive locators are FOSSA specific.
    #[strum(serialize = "archive")]
    Archive,

    /// Interacts with Bower.
    #[strum(serialize = "bower")]
    Bower,

    /// Interacts with Carthage.
    #[strum(serialize = "cart")]
    Cart,

    /// Interacts with Cargo.
    #[strum(serialize = "cargo")]
    Cargo,

    /// Interacts with Composer.
    #[strum(serialize = "comp")]
    Comp,

    /// Interacts with Conan.
    #[strum(serialize = "conan")]
    Conan,

    /// Interacts with Conda.
    #[strum(serialize = "conda")]
    Conda,

    /// Interacts with CPAN.
    #[strum(serialize = "cpan")]
    Cpan,

    /// Interacts with CRAN.
    #[strum(serialize = "cran")]
    Cran,

    /// The `custom` fetcher describes first party projects in FOSSA.
    ///
    /// These projects aren't really _fetched_;
    /// they're stored in FOSSA's database.
    #[strum(serialize = "custom")]
    Custom,

    /// Interacts with RubyGems.
    #[strum(serialize = "gem")]
    Gem,

    /// Interacts with git VCS hosts.
    #[strum(serialize = "git")]
    Git,

    /// Resolves 'git' dependencies in the same manner as Go modules.
    #[strum(serialize = "go")]
    Go,

    /// Interacts with Hackage.
    #[strum(serialize = "hackage")]
    Hackage,

    /// Interacts with Hex.
    #[strum(serialize = "hex")]
    Hex,

    /// Interacts with Maven.
    #[strum(serialize = "mvn")]
    Maven,

    /// Interacts with NPM.
    #[strum(serialize = "npm")]
    Npm,

    /// Interacts with Nuget.
    #[strum(serialize = "nuget")]
    Nuget,

    /// Interacts with PyPI.
    #[strum(serialize = "pip")]
    Pip,

    /// Interacts with CocoaPods.
    #[strum(serialize = "pod")]
    Pod,

    /// Interacts with Dart's package manager.
    #[strum(serialize = "pub")]
    Pub,

    /// Interact with Swift's package manager.
    #[strum(serialize = "swift")]
    Swift,

    /// Specifies an arbitrary URL,
    /// which is downloaded and treated like an `Archive` variant.
    #[strum(serialize = "url")]
    Url,

    /// A user-specified package.
    #[strum(serialize = "user")]
    User,
}
