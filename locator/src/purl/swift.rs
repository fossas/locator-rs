use crate::{Locator, Revision, ecosystems::Swift, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    let namespace = purl.namespace().ok_or_else(|| {
        super::Error::MissingNamespace("Swift PURL requires a namespace".to_string())
    })?;

    let package_name = format!("{}/{}", namespace, purl.name());
    let revision = purl.version().and_then(|v| Revision::parse(v).ok());

    Ok(Locator::builder()
        .ecosystem(Swift)
        .package(package_name)
        .maybe_revision(revision)
        .build())
}
