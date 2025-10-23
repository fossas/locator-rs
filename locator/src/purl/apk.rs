use crate::{Locator, Revision, ecosystems::LinuxAlpine, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Locator {
    let package_name = [
        Some(purl.name()),
        purl.namespace(),
        purl.qualifiers().get("distro"),
    ]
    .iter()
    .flatten()
    .cloned()
    .collect::<Vec<_>>()
    .join("#");

    let revision = [purl.qualifiers().get("arch"), purl.version()]
        .iter()
        .flatten()
        .cloned()
        .collect::<Vec<_>>()
        .join("#");

    Locator::builder()
        .ecosystem(LinuxAlpine)
        .package(package_name)
        .maybe_revision(Revision::parse(revision).ok())
        .build()
}
