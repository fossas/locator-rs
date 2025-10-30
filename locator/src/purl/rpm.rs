use crate::{Locator, Revision, ecosystems::LinuxRpm, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    // Distro examples: fedora-25 and centos-7.6.1810
    // Use 'latest' when distro version is unavailable to prevent API fail.
    let distro_version = purl
        .qualifiers()
        .get("distro")
        .and_then(|distro| distro.split('-').nth(1))
        .unwrap_or("latest");

    // Build package name: name#namespace#distro_version
    let package_name = [Some(purl.name()), purl.namespace(), Some(distro_version)]
        .iter()
        .flatten()
        .cloned()
        .collect::<Vec<_>>()
        .join("#");

    // Build revision: arch#epoch:version
    let version_with_epoch = purl.version().map(|v| {
        let epoch = purl.qualifiers().get("epoch").unwrap_or("0");
        format!("{epoch}:{v}")
    });

    let revision_parts = [purl.qualifiers().get("arch"), version_with_epoch.as_deref()];
    let revision = revision_parts
        .iter()
        .flatten()
        .cloned()
        .collect::<Vec<_>>()
        .join("#");

    Ok(Locator::builder()
        .ecosystem(LinuxRpm)
        .package(package_name)
        .maybe_revision(Revision::parse(revision).ok())
        .build())
}
