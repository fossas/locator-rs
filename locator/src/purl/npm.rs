use crate::{Locator, Revision, ecosystems::Npm, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Locator {
    let package_name = if let Some(namespace) = purl.namespace() {
        format!("{}/{}", namespace, purl.name())
    } else {
        purl.name().to_string()
    };

    let revision = purl.version().and_then(|v| Revision::parse(v).ok());

    Locator::builder()
        .ecosystem(Npm)
        .package(package_name)
        .maybe_revision(revision)
        .build()
}
