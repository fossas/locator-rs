use crate::{Fetcher, Locator, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    let package_name = if let Some(namespace) = purl.namespace() {
        format!("{}/{}", namespace, purl.name())
    } else {
        purl.name().to_string()
    };

    let revision = purl.version().and_then(super::revision);

    Ok(Locator::builder()
        .fetcher(Fetcher::Npm)
        .package(package_name)
        .maybe_revision(revision)
        .build())
}
