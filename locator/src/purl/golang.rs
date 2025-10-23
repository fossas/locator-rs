use crate::{Locator, Revision, ecosystems::Go, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Locator {
    let package_parts = [purl.namespace(), Some(purl.name()), purl.subpath()];
    let package_name = package_parts
        .iter()
        .flatten()
        .cloned()
        .collect::<Vec<_>>()
        .join("/");

    let revision = purl.version().and_then(|v| Revision::parse(v).ok());

    Locator::builder()
        .ecosystem(Go)
        .package(package_name)
        .maybe_revision(revision)
        .build()
}
