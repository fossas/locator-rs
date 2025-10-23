use crate::{Locator, Revision, ecosystems::Comp, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    let namespace = purl.namespace().ok_or_else(|| {
        super::Error::MissingNamespace(
            "Composer PURL requires a namespace (vendor name)".to_string(),
        )
    })?;

    let package_name = format!("{}/{}", namespace, purl.name());
    let revision = purl.version().and_then(|v| Revision::parse(v).ok());

    Ok(Locator::builder()
        .ecosystem(Comp)
        .package(package_name)
        .maybe_revision(revision)
        .build())
}
