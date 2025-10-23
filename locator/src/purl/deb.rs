use crate::{Locator, Revision, ecosystems::LinuxDebian, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    // Most Debian purl examples use code names instead of the numeric versions
    // (i.e., bullseye, buster, and stretch). On the other hand, Ubuntu uses
    // numeric versions (i.e., ubuntu-20.04 and ubuntu-22.04).
    let distro_version = purl
        .qualifiers()
        .get("distro")
        .and_then(|distro| {
            if distro.contains('-') {
                distro.split('-').nth(1)
            } else {
                Some(distro)
            }
        })
        .ok_or_else(|| super::Error::MissingQualifier("distro".to_string()))?;

    // Build package name: name#namespace#distro_version
    let package_name = [Some(purl.name()), purl.namespace(), Some(distro_version)]
        .iter()
        .flatten()
        .cloned()
        .collect::<Vec<_>>()
        .join("#");

    // Build revision: arch#version
    let revision = [purl.qualifiers().get("arch"), purl.version()]
        .iter()
        .flatten()
        .cloned()
        .collect::<Vec<_>>()
        .join("#");

    Ok(Locator::builder()
        .ecosystem(LinuxDebian)
        .package(package_name)
        .maybe_revision(Revision::parse(revision).ok())
        .build())
}
