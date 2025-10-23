use crate::{Locator, Revision, ecosystems::Comp, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Locator {
    let package_name = format!("{}/{}", purl.namespace().unwrap_or(""), purl.name());

    let revision = purl.version().and_then(|v| Revision::parse(v).ok());

    Locator::builder()
        .ecosystem(Comp)
        .package(package_name)
        .maybe_revision(revision)
        .build()
}
