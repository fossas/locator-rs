use crate::{Fetcher, Locator, Revision, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    let mut package_parts = Vec::new();

    // If repository_url qualifier exists, include it first
    if let Some(repository_url) = purl.qualifiers().get("repository_url") {
        package_parts.push(repository_url);
    }

    // Always include namespace and name
    if let Some(namespace) = purl.namespace() {
        package_parts.push(namespace);
    }
    package_parts.push(purl.name());

    let package_name = package_parts.join(":");
    let revision = purl.version().map(Revision::from);

    Ok(Locator::builder()
        .fetcher(Fetcher::Maven)
        .package(package_name)
        .maybe_revision(revision)
        .build())
}
