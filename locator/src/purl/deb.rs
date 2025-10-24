use crate::{
    Locator, Revision,
    ecosystems::LinuxDebian,
    purl::{ConversionOptions, Purl},
};

pub fn purl_to_locator(purl: Purl, options: ConversionOptions) -> Result<Locator, super::Error> {
    let distro_name = purl
        .namespace()
        .or(options.fallback_deb_distro_name.as_deref())
        .ok_or(super::Error::MissingQualifier("distro".to_string()))?;

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
        .or(options.fallback_deb_distro_version.as_deref())
        .ok_or(super::Error::MissingQualifier("distro".to_string()))?;

    // Build package name: name#namespace#distro_version
    let package_name = [purl.name(), distro_name, distro_version].join("#");

    // Build revision: arch#version
    let arch = purl
        .qualifiers()
        .get("arch")
        .or(options.fallback_deb_arch.as_deref());
    let revision = [arch, purl.version()]
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
