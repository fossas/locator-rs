use crate::{Fetcher, Locator, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    let package_parts = [purl.namespace(), Some(purl.name()), purl.subpath()];
    let package_name = package_parts
        .iter()
        .flatten()
        .cloned()
        .collect::<Vec<_>>()
        .join("/");

    let revision = purl.version().and_then(super::revision);

    Ok(Locator::builder()
        .fetcher(Fetcher::Go)
        .package(package_name)
        .maybe_revision(revision)
        .build())
}
