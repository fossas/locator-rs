use crate::{Locator, Revision, ecosystems::Pip, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Locator {
    let package_name = purl.name();
    let revision = purl.version().and_then(|v| Revision::parse(v).ok());

    Locator::builder()
        .ecosystem(Pip)
        .package(package_name)
        .maybe_revision(revision)
        .build()
}
