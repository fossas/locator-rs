use crate::{Locator, Revision, ecosystems::SourceForge, purl::Purl};

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    // SourceForge PURLs can be:
    // pkg:sourceforge/pkgname@1.0
    // or
    // pkg:sourceforge/pkgname/subpkgname@1.0
    // if there is no subpkgname, then the namespace will be the name.
    let package_name = purl.namespace().unwrap_or(purl.name());
    let revision = purl.version().and_then(|v| Revision::parse(v).ok());

    Ok(Locator::builder()
        .ecosystem(SourceForge)
        .package(package_name)
        .maybe_revision(revision)
        .build())
}
