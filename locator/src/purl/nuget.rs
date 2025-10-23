use crate::{Locator, Revision, ecosystems::Nuget, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    let package_name = purl.name();
    let revision = purl.version().and_then(|v| Revision::parse(v).ok());

    Ok(Locator::builder()
        .ecosystem(Nuget)
        .package(package_name)
        .maybe_revision(revision)
        .build())
}
