use crate::{Fetcher, Locator, Revision, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    // CPAN locators use `::` as separator (e.g., `Tk::Tree`)
    // while PURLs use `-` (e.g., `Tk-Tree`).
    // The namespace is not used as multiple authors can maintain the same
    // package over time.
    let package_name = purl.name().replace('-', "::");
    let revision = purl.version().map(Revision::from);

    Ok(Locator::builder()
        .fetcher(Fetcher::Cpan)
        .package(package_name)
        .maybe_revision(revision)
        .build())
}
