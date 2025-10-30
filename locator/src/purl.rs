//! Logic for converting Package URLs (PURLs) to [`Locators`](Locator).
//!
//! See the [Package URL specification](https://github.com/package-url/purl-spec).
//!
//! Exposes the [`Purl`] struct, which is a thin wrapper around [`purl::GenericPurl`].
//! This struct can be converted to a [`Locator`] via the [`TryFrom`] trait.
//!
//! Not all PURL types are supported. Unsupported PURLs will return an
//! [`Error::UnsupportedPurl`] error. See the [`Locator::try_from`] impl in this
//! module for the full list of supported PURL types.

use std::str::FromStr;

use derive_more::{Deref, DerefMut, From};
use purl::GenericPurl;
use thiserror::Error;

use crate::Locator;

mod apk;
mod bitbucket;
mod cocoapods;
mod composer;
mod cpan;
mod deb;
mod gitee;
mod github;
mod gitlab;
mod golang;
mod googlesource;
mod maven;
mod npm;
mod rpm;
mod sourceforge;
mod swift;

/// A Package URL (PURL).
///
/// A package URL is a standardized way to identify and locate software packages.
/// It's very similar to a locator, but has a different format and set of conventions.
///
/// Read more about PURLs in the [spec](https://github.com/package-url/purl-spec).
///
/// This struct is a thin wrapper around [`purl::GenericPurl`], which is an
/// external crate implementation of the PURL spec, and may have its own
/// limitations.
///
/// The main purpose of this struct is to provide a way to convert a PURL
/// to a [`Locator`]. You can start by parsing a PURL from a string:
/// ```rust
/// # use locator::purl::Purl;
/// # use std::str::FromStr;
/// let purl = Purl::from_str("pkg:npm/lodash@4.17.21").unwrap();
/// ```
/// Then convert it to a locator:
/// ```rust
/// # use locator::purl::Purl;
/// # use locator::Locator;
/// # use std::str::FromStr;
/// # use std::convert::TryFrom;
/// # let purl = Purl::from_str("pkg:npm/lodash@4.17.21").unwrap();
/// let locator = Locator::try_from(purl).unwrap();
/// ```
///
/// Another way to convert is via the [`Purl::try_into_locator_with_options`]
/// which allows providing custom [`ConversionOptions`] that can modify the
/// behaviour of the conversion for specific ecosystems:
/// ```rust
/// # use locator::purl::{Purl, ConversionOptions};
/// # use locator::Locator;
/// # use std::str::FromStr;
/// // We're missing the namespace in this PURL, but we can provide a fallback.
/// let purl = Purl::from_str("pkg:composer/laravel@5.5.0").unwrap();
/// let options = ConversionOptions {
///     fallback_composer_namespace: Some("vendor".to_string()),
///     ..Default::default()
/// };
/// let locator = purl.try_into_locator_with_options(options).unwrap();
/// ```
///
/// All of these operations can fail. See [`purl::ParseError`] for PURL parsing
/// errors and [`Error`] for conversion errors.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Deref, DerefMut, From)]
pub struct Purl(GenericPurl<String>);

impl FromStr for Purl {
    type Err = purl::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let generic_purl = GenericPurl::from_str(s)?;
        Ok(Purl(generic_purl))
    }
}

/// Try to convert a Purl to a Locator. This is a fallible operation, see
/// the [`Error`] enum for possible errors.
impl TryFrom<Purl> for Locator {
    type Error = Error;

    fn try_from(purl: Purl) -> Result<Self, Self::Error> {
        purl.try_into_locator_with_options(ConversionOptions::default())
    }
}

impl Purl {
    /// Determines the corresponding [`Ecosystem`](crate::Ecosystem) for this PURL.
    /// This is based on the PURL package type.
    ///
    /// E.g.
    /// ```rust
    /// # use locator::purl::Purl;
    /// # use std::str::FromStr;
    /// let purl = Purl::from_str("pkg:npm/lodash@4.17.21").unwrap();
    /// let ecosystem = purl.ecosystem().unwrap();
    /// assert_eq!(ecosystem, locator::Ecosystem::Npm);
    /// ```
    ///
    /// This is a fallible operation, see the [`Error`] enum for possible errors.
    pub fn ecosystem(&self) -> Result<crate::Ecosystem, Error> {
        let package_type = self.package_type().to_string();

        macro_rules! unsupported {
            () => {
                Err(Error::UnsupportedPurl(package_type.clone()))
            };
        }

        match package_type.as_str() {
            "alpm" => unsupported!(),
            "apk" => Ok(crate::Ecosystem::LinuxAlpine),
            "bitbucket" => Ok(crate::Ecosystem::Git),
            "bower" => unsupported!(),
            "cargo" => Ok(crate::Ecosystem::Cargo),
            "carthage" => unsupported!(),
            "cocoapods" => Ok(crate::Ecosystem::Pod),
            "composer" => Ok(crate::Ecosystem::Comp),
            "conan" => unsupported!(),
            "conda" => unsupported!(),
            "cpan" => Ok(crate::Ecosystem::Cpan),
            "cran" => Ok(crate::Ecosystem::Cran),
            "deb" => Ok(crate::Ecosystem::LinuxDebian),
            "gem" => Ok(crate::Ecosystem::Gem),
            "generic" => unsupported!(),
            "github" => Ok(crate::Ecosystem::Git),
            "gitee" => Ok(crate::Ecosystem::Git),
            "gitlab" => Ok(crate::Ecosystem::Git),
            "golang" => Ok(crate::Ecosystem::Go),
            "googlesource" => Ok(crate::Ecosystem::Git),
            "gradle" => unsupported!(),
            "hackage" => Ok(crate::Ecosystem::Hackage),
            "hex" => Ok(crate::Ecosystem::Hex),
            "maven" => Ok(crate::Ecosystem::Maven),
            "npm" => Ok(crate::Ecosystem::Npm),
            "nuget" => Ok(crate::Ecosystem::Nuget),
            "pub" => Ok(crate::Ecosystem::Pub),
            "pypi" => Ok(crate::Ecosystem::Pip),
            "rpm" => Ok(crate::Ecosystem::LinuxRpm),
            "sourceforge" => Ok(crate::Ecosystem::SourceForge),
            "stackoverflow" => Ok(crate::Ecosystem::StackOverflow),
            "swift" => Ok(crate::Ecosystem::Swift),
            _ => unsupported!(),
        }
    }

    /// Converts a [`Purl`] into a [`Locator`] using the provided [`ConversionOptions`].
    ///
    /// The conversion options allow customizing the conversion process for
    /// specific ecosystems. If you don't need any custom options, you can use
    /// the default conversion via the [`TryFrom`] trait implementation.
    ///
    /// This is a fallible operation, see the [`Error`] enum for possible errors.
    pub fn try_into_locator_with_options(
        self,
        options: ConversionOptions,
    ) -> Result<Locator, Error> {
        let package_type = self.package_type().to_string();

        macro_rules! unsupported {
            () => {
                Err(Error::UnsupportedPurl(package_type.clone()))
            };
        }

        match package_type.as_str() {
            "alpm" => unsupported!(),
            "apk" => apk::purl_to_locator(self),
            "bitbucket" => bitbucket::purl_to_locator(self),
            "bower" => unsupported!(),
            "cargo" => self.to_default_locator(),
            "carthage" => unsupported!(),
            "cocoapods" => cocoapods::purl_to_locator(self),
            "composer" => composer::purl_to_locator(self, options),
            "conan" => unsupported!(),
            "conda" => unsupported!(),
            "cpan" => cpan::purl_to_locator(self),
            "cran" => self.to_default_locator(),
            "deb" => deb::purl_to_locator(self, options),
            "gem" => self.to_default_locator(),
            "generic" => unsupported!(),
            "github" => github::purl_to_locator(self),
            "gitee" => gitee::purl_to_locator(self),
            "gitlab" => gitlab::purl_to_locator(self),
            "golang" => golang::purl_to_locator(self),
            "googlesource" => googlesource::purl_to_locator(self),
            "gradle" => unsupported!(),
            "hackage" => self.to_default_locator(),
            "hex" => self.to_default_locator(),
            "maven" => maven::purl_to_locator(self),
            "npm" => npm::purl_to_locator(self),
            "nuget" => self.to_default_locator(),
            "pub" => self.to_default_locator(),
            "pypi" => self.to_default_locator(),
            "rpm" => rpm::purl_to_locator(self),
            "sourceforge" => sourceforge::purl_to_locator(self),
            "stackoverflow" => self.to_default_locator(),
            "swift" => swift::purl_to_locator(self, options),
            _ => unsupported!(),
        }
    }

    /// Converts a [`Purl`] into a [`Locator`] using a default conversion method.
    /// This should be left private while [`Purl::try_into_locator_with_options`]
    /// is the preferred public method for conversion because most ecosystems
    /// require special handling that the default method does not provide.
    ///
    /// This is a fallible operation, see the [`Error`] enum for possible errors.
    fn to_default_locator(&self) -> Result<Locator, Error> {
        let ecosystem = self.ecosystem()?;
        let package = self.name();
        let revision = self.version().and_then(|v| crate::Revision::parse(v).ok());

        Ok(Locator::builder()
            .ecosystem(ecosystem)
            .package(package)
            .maybe_revision(revision)
            .build())
    }
}

/// Options for converting a PURL to a Locator.
///
/// These options customize the behaviour of the conversation process for
/// specific ecosystems.
///
/// The `fallback_*` fields provide default values to use when the corresponding
/// parts are missing from the PURL. It's useful to provide these when you want
/// the conversation to succeed even if the PURL is incomplete.
#[derive(Debug, Clone, Default)]
pub struct ConversionOptions {
    /// Fallback Debian distro name to use if the PURL namespace is missing.
    pub fallback_deb_distro_name: Option<String>,
    /// Fallback Debian distro version to use if the `distro` qualifier is missing.
    pub fallback_deb_distro_version: Option<String>,
    /// Fallback Debian architecture to use if the `arch` qualifier is missing.
    pub fallback_deb_arch: Option<String>,
    /// Fallback Swift namespace to use if the PURL namespace is missing.
    pub fallback_swift_namespace: Option<String>,
    /// Fallback Composer namespace to use if the PURL namespace is missing.
    pub fallback_composer_namespace: Option<String>,
}

/// Errors that can occur when converting a PURL to a locator.
#[derive(Error, Debug)]
pub enum Error {
    /// The PURL type is not supported for conversion to a locator.
    #[error("unsupported purl: {0}")]
    UnsupportedPurl(String),
    /// A required qualifier is missing from the PURL.
    #[error("missing required qualifier: {0}")]
    MissingQualifier(String),
    /// A required namespace is missing from the PURL.
    #[error("missing required namespace: {0}")]
    MissingNamespace(String),
}

#[cfg(test)]
mod tests {
    use simple_test_case::test_case;

    use super::*;

    #[test_case("pkg:apk/alpine/alpine-keys@1.1-r0?distro=3.4.6", "apk+alpine-keys#alpine#3.4.6$1.1-r0"; "apk_with_distro")]
    #[test_case("pkg:apk/alpine/alpine-baselayout-data@3.4.0-r0", "apk+alpine-baselayout-data#alpine$3.4.0-r0"; "apk_without_distro")]
    #[test_case("pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1", "mvn+org.apache.xmlgraphics:batik-anim$1.9.1"; "maven_basic")]
    #[test_case("pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1?repository_url=repo.spring.io%2Frelease", "mvn+repo.spring.io/release:org.apache.xmlgraphics:batik-anim$1.9.1"; "maven_with_repository_url")]
    #[test_case("pkg:cargo/structopt@0.3.11", "cargo+structopt$0.3.11"; "cargo_basic")]
    #[test_case("pkg:composer/laravel/laravel@5.5.0", "comp+laravel/laravel$5.5.0"; "composer_basic")]
    #[test_case("pkg:cpan/CTDEAN/Tk-Tree@0.02", "cpan+Tk::Tree$0.02"; "cpan_with_namespace")]
    #[test_case("pkg:cpan/Convert-ASCIINames@1.002", "cpan+Convert::ASCIINames$1.002"; "cpan_without_namespace")]
    #[test_case("pkg:cran/caret@6.0-88", "cran+caret$6.0-88"; "cran_basic")]
    #[test_case("pkg:gem/ruby-advisory-db-check@0.12.4", "gem+ruby-advisory-db-check$0.12.4"; "gem_basic")]
    #[test_case("pkg:golang/github.com/gorilla/context@234fd47e07d1004f0aed9c", "go+github.com/gorilla/context$234fd47e07d1004f0aed9c"; "golang_basic")]
    #[test_case("pkg:golang/github.com/gorilla/context@234fd47e07d1004f0aed9c#api", "go+github.com/gorilla/context/api$234fd47e07d1004f0aed9c"; "golang_with_subpath")]
    #[test_case("pkg:hackage/a50@0.5", "hackage+a50$0.5"; "hackage_basic")]
    #[test_case("pkg:hex/jason@1.1.2", "hex+jason$1.1.2"; "hex_basic")]
    #[test_case("pkg:npm/foobar@12.3.1", "npm+foobar$12.3.1"; "npm_basic")]
    #[test_case("pkg:npm/%40angular/animation@12.3.1", "npm+@angular/animation$12.3.1"; "npm_with_namespace")]
    #[test_case("pkg:nuget/EnterpriseLibrary.Common@6.0.1304", "nuget+EnterpriseLibrary.Common$6.0.1304"; "nuget_basic")]
    #[test_case("pkg:pypi/django-allauth@12.23", "pip+django-allauth$12.23"; "pypi_basic")]
    #[test_case("pkg:pub/at_persistence_secondary_server@1.2.0", "pub+at_persistence_secondary_server$1.2.0"; "pub_basic")]
    #[test_case("pkg:swift/github.com/Alamofire/Alamofire@5.4.3", "swift+github.com/Alamofire/Alamofire$5.4.3"; "swift_basic")]
    #[test_case("pkg:cocoapods/MapsIndoors@3.24.0", "pod+MapsIndoors$3.24.0"; "cocoapods_basic")]
    #[test_case("pkg:cocoapods/ShareKit@2.0#Twitter", "pod+ShareKit/Twitter$2.0"; "cocoapods_with_subspec")]
    #[test_case("pkg:github/package-url/purl-spec@244fd47e07d1004", "git+github.com/package-url/purl-spec$244fd47e07d1004"; "github_basic")]
    #[test_case("pkg:gitlab/NTPsec/ntpsec@1feb8f90dd30ae573bf79205af53879a6e1e60c4", "git+gitlab.com/NTPsec/ntpsec$1feb8f90dd30ae573bf79205af53879a6e1e60c4"; "gitlab_basic")]
    #[test_case("pkg:gitee/apache/thrift@0.11.0", "git+gitee.com/apache/thrift$0.11.0"; "gitee_basic")]
    #[test_case("pkg:googlesource/android/device/linaro/bootloader/edk2@aml_tz2_306503000", "git+android.googlesource.com/device/linaro/bootloader/edk2$aml_tz2_306503000"; "googlesource_basic")]
    #[test_case("pkg:bitbucket/birkenfeld/pygments-main@244fd47e07d1014f0aed9c", "git+bitbucket.org/birkenfeld/pygments-main$244fd47e07d1014f0aed9c"; "bitbucket_basic")]
    #[test_case("pkg:rpm/fedora/curl@7.50.3-1.fc25?arch=i386&distro=fedora-25", "rpm-generic+curl#fedora#25$i386#0:7.50.3-1.fc25"; "rpm_basic")]
    #[test_case("pkg:rpm/fedora/curl@7.50.3-1.fc25?epoch=1&arch=i386&distro=fedora-25", "rpm-generic+curl#fedora#25$i386#1:7.50.3-1.fc25"; "rpm_with_epoch")]
    #[test_case("pkg:rpm/fedora/curl@7.50.3-1.fc25?epoch=1&arch=i386", "rpm-generic+curl#fedora#latest$i386#1:7.50.3-1.fc25"; "rpm_without_distro")]
    #[test_case("pkg:deb/debian/dpkg@1.19.0.4?arch=amd64&distro=stretch", "deb+dpkg#debian#stretch$amd64#1.19.0.4"; "deb_basic")]
    #[test_case("pkg:deb/ubuntu/adduser@3.118ubuntu2?distro=ubuntu-20.04&arch=amd64", "deb+adduser#ubuntu#20.04$amd64#3.118ubuntu2"; "deb_with_vendor_version")]
    #[test_case("pkg:sourceforge/dex-os@1.0", "sourceforge+dex-os$1.0"; "sourceforge_without_namespace")]
    #[test_case("pkg:sourceforge/intel-iscsi/ost@3.0.0", "sourceforge+intel-iscsi$3.0.0"; "sourceforge_with_namespace")]
    #[test_case("pkg:stackoverflow/48810170@1", "stackoverflow+48810170$1"; "stackoverflow_basic")]
    #[test]
    fn purl_to_locator(purl_str: &str, locator_str: &str) {
        let purl = Purl::from_str(purl_str).expect("parse purl");
        let locator = Locator::try_from(purl).expect("convert to locator");
        assert_eq!(locator.to_string(), locator_str);
    }

    #[test_case(None, None, None, "pkg:deb/ubuntu/adduser@3.118ubuntu2?distro=ubuntu-20.04&arch=amd64", "deb+adduser#ubuntu#20.04$amd64#3.118ubuntu2"; "no_options")]
    #[test_case(None, Some("unknown"), None, "pkg:deb/ubuntu/adduser@3.118ubuntu2?arch=amd64", "deb+adduser#ubuntu#unknown$amd64#3.118ubuntu2"; "fallback_version")]
    #[test_case(Some("debian"), Some("unknown"), None, "pkg:deb/adduser@3.118ubuntu2?arch=amd64", "deb+adduser#debian#unknown$amd64#3.118ubuntu2"; "fallback_name_and_version")]
    #[test_case(None, None, Some("unknown"), "pkg:deb/ubuntu/adduser@3.118ubuntu2?distro=ubuntu-20.04", "deb+adduser#ubuntu#20.04$unknown#3.118ubuntu2"; "fallback_arch")]
    #[test_case(Some("debian"), Some("unknown"), None, "pkg:deb/ubuntu/adduser@3.118ubuntu2?arch=amd64&distro=ubuntu-20.04", "deb+adduser#ubuntu#20.04$amd64#3.118ubuntu2"; "unnecesssary_fallback_name_and_version")]
    #[test_case(None, None, Some("unknown"), "pkg:deb/ubuntu/adduser@3.118ubuntu2?distro=ubuntu-20.04&arch=amd64", "deb+adduser#ubuntu#20.04$amd64#3.118ubuntu2"; "unnecesssary_fallback_arch")]
    #[test]
    fn deb_options(
        fallback_name: Option<&str>,
        fallback_version: Option<&str>,
        fallback_arch: Option<&str>,
        purl_str: &str,
        locator_str: &str,
    ) {
        let purl = Purl::from_str(purl_str).expect("parse purl");
        let options = ConversionOptions {
            fallback_deb_distro_name: fallback_name.map(String::from),
            fallback_deb_distro_version: fallback_version.map(String::from),
            fallback_deb_arch: fallback_arch.map(String::from),
            ..Default::default()
        };
        let locator = purl
            .try_into_locator_with_options(options)
            .expect("convert to locator");
        assert_eq!(locator.to_string(), locator_str);
    }

    #[test_case(None, "pkg:swift/github.com/Alamofire/Alamofire@5.4.3", "swift+github.com/Alamofire/Alamofire$5.4.3"; "no_fallback")]
    #[test_case(Some("github.com/example"), "pkg:swift/Alamofire@5.4.3", "swift+github.com/example/Alamofire$5.4.3"; "with_fallback")]
    #[test_case(Some("github.com/example"), "pkg:swift/github.com/Alamofire/Alamofire@5.4.3", "swift+github.com/Alamofire/Alamofire$5.4.3"; "unnecessary_fallback")]
    #[test]
    fn swift_options(fallback_namespace: Option<&str>, purl_str: &str, locator_str: &str) {
        let purl = Purl::from_str(purl_str).expect("parse purl");
        let options = ConversionOptions {
            fallback_swift_namespace: fallback_namespace.map(String::from),
            ..Default::default()
        };
        let locator = purl
            .try_into_locator_with_options(options)
            .expect("convert to locator");
        assert_eq!(locator.to_string(), locator_str);
    }

    #[test_case(None, "pkg:composer/laravel/laravel@5.5.0", "comp+laravel/laravel$5.5.0"; "no_fallback")]
    #[test_case(Some("vendor"), "pkg:composer/laravel@5.5.0", "comp+vendor/laravel$5.5.0"; "with_fallback")]
    #[test_case(Some("vendor"), "pkg:composer/laravel/laravel@5.5.0", "comp+laravel/laravel$5.5.0"; "unnecessary_fallback")]
    #[test]
    fn composer_options(fallback_namespace: Option<&str>, purl_str: &str, locator_str: &str) {
        let purl = Purl::from_str(purl_str).expect("parse purl");
        let options = ConversionOptions {
            fallback_composer_namespace: fallback_namespace.map(String::from),
            ..Default::default()
        };
        let locator = purl
            .try_into_locator_with_options(options)
            .expect("convert to locator");
        assert_eq!(locator.to_string(), locator_str);
    }

    #[test_case("pkg:swift/Alamofire@5.4.3", "Swift PURL requires a namespace"; "swift_missing_namespace")]
    #[test_case("pkg:composer/laravel@5.5.0", "Composer PURL requires a namespace (vendor name)"; "composer_missing_namespace")]
    #[test_case("pkg:googlesource/edk2@aml_tz2_306503000", "GoogleSource PURL must include a subdomain in the namespace"; "googlesource_missing_namespace")]
    #[test]
    fn missing_namespace_error(purl_str: &str, expected_msg: &str) {
        let purl = Purl::from_str(purl_str).expect("parse purl");
        let result = Locator::try_from(purl);
        assert!(result.is_err());
        match result {
            Err(Error::MissingNamespace(msg)) => {
                assert_eq!(msg, expected_msg);
            }
            _ => panic!("Expected MissingNamespace error, got: {result:?}"),
        }
    }

    #[test_case("pkg:deb/ubuntu/adduser@3.118ubuntu2?arch=amd64", "distro"; "deb_missing_distro")]
    #[test]
    fn missing_qualifier_error(purl_str: &str, expected_qualifier: &str) {
        let purl = Purl::from_str(purl_str).expect("parse purl");
        let result = Locator::try_from(purl);
        assert!(result.is_err());
        match result {
            Err(Error::MissingQualifier(msg)) => {
                assert_eq!(msg, expected_qualifier);
            }
            _ => panic!("Expected MissingQualifier error, got: {result:?}"),
        }
    }

    #[test_case("pkg:foo/foo@1.0.0", "foo"; "unsupported")]
    #[test]
    fn unsupported_purl_error(purl_str: &str, expected_type: &str) {
        let purl = Purl::from_str(purl_str).expect("parse purl");
        let result = Locator::try_from(purl);
        assert!(result.is_err());
        match result {
            Err(Error::UnsupportedPurl(msg)) => {
                assert_eq!(msg, expected_type);
            }
            _ => panic!("Expected UnsupportedPurl error, got: {result:?}"),
        }
    }
}
