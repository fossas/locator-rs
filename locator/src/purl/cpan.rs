use crate::{Locator, Revision, ecosystems::Cpan, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Locator {
    // CPAN locators use `::` as separator (e.g., `Tk::Tree`)
    // while PURLs use `-` (e.g., `Tk-Tree`).
    // The namespace is not used as multiple authors can maintain the same
    // package over time.
    let package_name = purl.name().replace('-', "::");
    let revision = purl.version().and_then(|v| Revision::parse(v).ok());

    Locator::builder()
        .ecosystem(Cpan)
        .package(package_name)
        .maybe_revision(revision)
        .build()
}
