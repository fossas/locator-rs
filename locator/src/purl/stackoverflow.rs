use crate::{Locator, Revision, ecosystems::StackOverflow, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    // StackOverflow PURLs look like: pkg:stackoverflow/48810170@1
    // where the name is the post ID and version is the revision number.
    let package_name = purl.name();
    let revision = purl.version().and_then(|v| Revision::parse(v).ok());

    Ok(Locator::builder()
        .ecosystem(StackOverflow)
        .package(package_name)
        .maybe_revision(revision)
        .build())
}
