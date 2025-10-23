use crate::{Locator, Revision, ecosystems::Git, purl::Purl};

pub const DOMAIN: &str = "googlesource.com";

pub fn purl_to_locator(purl: Purl) -> Result<Locator, super::Error> {
    // GoogleSource PURLs have the subdomain as the first part of namespace.
    // e.g., pkg:googlesource/android/device/linaro/bootloader/edk2
    // becomes android.googlesource.com/device/linaro/bootloader/edk2
    let namespace_parts = purl
        .namespace()
        .map(|ns| ns.split('/').collect::<Vec<_>>())
        .unwrap_or_default();

    let subdomain = namespace_parts.first().ok_or_else(|| {
        super::Error::MissingNamespace(
            "GoogleSource PURL must include a subdomain in the namespace".to_string(),
        )
    })?;
    let remaining_namespace = &namespace_parts[1..];

    let mut package_parts = vec![format!("{}.{}", subdomain, DOMAIN)];
    package_parts.extend(remaining_namespace.iter().map(|s| s.to_string()));
    package_parts.push(purl.name().to_string());

    let package_name = package_parts.join("/");
    let revision = purl.version().and_then(|v| Revision::parse(v).ok());

    Ok(Locator::builder()
        .ecosystem(Git)
        .package(package_name)
        .maybe_revision(revision)
        .build())
}
