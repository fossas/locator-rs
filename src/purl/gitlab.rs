use crate::{Fetcher, Locator, Revision, purl::Purl};

pub const DOMAIN: &str = "gitlab.com";

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    let package_name = [Some(DOMAIN), purl.namespace(), Some(purl.name())]
        .iter()
        .flatten()
        .cloned()
        .collect::<Vec<_>>()
        .join("/");
    let revision = purl.version().map(Revision::from);

    Ok(Locator::builder()
        .fetcher(Fetcher::Git)
        .package(package_name)
        .maybe_revision(revision)
        .build())
}
