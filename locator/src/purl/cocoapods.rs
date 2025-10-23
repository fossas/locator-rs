use crate::{Locator, Revision, ecosystems::Pod, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    let package_parts = [Some(purl.name()), purl.subpath()];
    let package_name = package_parts
        .iter()
        .flatten()
        .cloned()
        .collect::<Vec<_>>()
        .join("/");

    let revision = purl.version().and_then(|v| Revision::parse(v).ok());

    Ok(Locator::builder()
        .ecosystem(Pod)
        .package(package_name)
        .maybe_revision(revision)
        .build())
}
