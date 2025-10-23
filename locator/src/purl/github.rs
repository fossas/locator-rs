use crate::{Locator, Revision, ecosystems::Git, purl::Purl};

pub const DOMAIN: &str = "github.com";

pub fn purl_to_locator(purl: Purl) -> Locator {
    let package_name = [Some(DOMAIN), purl.namespace(), Some(purl.name())]
        .iter()
        .flatten()
        .cloned()
        .collect::<Vec<_>>()
        .join("/");
    let revision = purl.version().and_then(|v| Revision::parse(v).ok());

    Locator::builder()
        .ecosystem(Git)
        .package(package_name)
        .maybe_revision(revision)
        .build()
}
