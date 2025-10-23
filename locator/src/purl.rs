//! Logic for converting locators to and from a package URL (PURL) format.

use std::str::FromStr;

use derive_more::{Deref, DerefMut};
use purl::GenericPurl;

use crate::Locator;

mod apk;
mod bitbucket;
mod cargo;
mod cocoapods;
mod composer;
mod cpan;
mod cran;
mod dart_pub;
mod deb;
mod gem;
mod gitee;
mod github;
mod gitlab;
mod golang;
mod googlesource;
mod hackage;
mod hex;
mod maven;
mod npm;
mod nuget;
mod pypi;
mod rpm;
mod sourceforge;
mod stackoverflow;
mod swift;

/// A Package URL (PURL).
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Deref, DerefMut)]
pub struct Purl(GenericPurl<String>);

impl FromStr for Purl {
    type Err = purl::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let generic_purl = GenericPurl::from_str(s)?;
        Ok(Purl(generic_purl))
    }
}

impl From<Purl> for Locator {
    fn from(purl: Purl) -> Self {
        match purl.package_type().as_str() {
            "alpm" => todo!(),
            "apk" => apk::purl_to_locator(purl),
            "bitbucket" => bitbucket::purl_to_locator(purl),
            "bower" => todo!(),
            "cargo" => cargo::purl_to_locator(purl),
            "carthage" => todo!(),
            "cocoapods" => cocoapods::purl_to_locator(purl),
            "composer" => composer::purl_to_locator(purl),
            "conan" => todo!(),
            "conda" => todo!(),
            "cpan" => cpan::purl_to_locator(purl),
            "cran" => cran::purl_to_locator(purl),
            "deb" => deb::purl_to_locator(purl),
            "gem" => gem::purl_to_locator(purl),
            "generic" => todo!(),
            "github" => github::purl_to_locator(purl),
            "gitee" => gitee::purl_to_locator(purl),
            "gitlab" => gitlab::purl_to_locator(purl),
            "golang" => golang::purl_to_locator(purl),
            "googlesource" => googlesource::purl_to_locator(purl),
            "gradle" => todo!(),
            "hackage" => hackage::purl_to_locator(purl),
            "hex" => hex::purl_to_locator(purl),
            "maven" => maven::purl_to_locator(purl),
            "npm" => npm::purl_to_locator(purl),
            "nuget" => nuget::purl_to_locator(purl),
            "pub" => dart_pub::purl_to_locator(purl),
            "pypi" => pypi::purl_to_locator(purl),
            "rpm" => rpm::purl_to_locator(purl),
            "sourceforge" => sourceforge::purl_to_locator(purl),
            "stackoverflow" => stackoverflow::purl_to_locator(purl),
            "swift" => swift::purl_to_locator(purl),
            _ => todo!(),
        }
    }
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
        let purl = Purl::from_str(purl_str).unwrap();
        let locator: Locator = purl.into();
        assert_eq!(locator.to_string(), locator_str);
    }
}
