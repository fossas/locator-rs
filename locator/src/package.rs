use std::borrow::Cow;

use compact_str::CompactString;
use derive_more::{Debug, Display};
use documented::Documented;
use serde::{Deserialize, Serialize};
use serde_json::json;
use utoipa::{
    PartialSchema, ToSchema,
    openapi::{ObjectBuilder, Type},
};

/// The package section of the locator.
///
/// A "package" is generally the name of a project or dependency in a code host.
/// However some ecosystem ecosystems (such as `git`) embed additional information
/// inside the `Package` of a locator, such as the URL of the `git` instance
/// from which the project can be fetched.
///
/// Additionally, some ecosystem ecosystems (such as `apk`, `rpm-generic`, and `deb`)
/// further encode additional standardized information in the `Package` of the locator.
#[derive(Clone, Eq, PartialEq, Hash, Display, Debug, Serialize, Deserialize, Documented)]
#[display("{}", self.0)]
pub struct Package(CompactString);

impl Package {
    /// View the item as a string.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl<S: Into<CompactString>> From<S> for Package {
    fn from(value: S) -> Self {
        Self(value.into())
    }
}

impl From<&Package> for Package {
    fn from(value: &Package) -> Self {
        value.clone()
    }
}

impl std::cmp::Ord for Package {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        alphanumeric_sort::compare_str(&self.0, &other.0)
    }
}

impl std::cmp::PartialOrd for Package {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialSchema for Package {
    fn schema() -> utoipa::openapi::RefOr<utoipa::openapi::schema::Schema> {
        ObjectBuilder::new()
            .description(Some(Self::DOCS))
            .examples([json!("github.com/fossas/locator-rs"), json!("lodash")])
            .min_length(Some(1))
            .schema_type(Type::String)
            .build()
            .into()
    }
}

impl ToSchema for Package {
    fn name() -> Cow<'static, str> {
        Cow::Borrowed("Package")
    }
}
