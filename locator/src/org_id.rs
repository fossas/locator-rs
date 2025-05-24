use std::{num::ParseIntError, str::FromStr};

use derive_more::{Debug, Display};
use documented::Documented;
use duplicate::duplicate;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

/// Identifies the organization to which this locator is namespaced.
///
/// Organization IDs are canonically created by FOSSA instances
/// and have no meaning outside of FOSSA instances.
#[derive(
    Copy,
    Clone,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Serialize,
    Deserialize,
    Hash,
    Documented,
    ToSchema,
    Display,
    Debug,
)]
#[schema(example = json!(1))]
#[display("{}", self.0)]
pub struct OrgId(usize);

impl From<OrgId> for usize {
    fn from(value: OrgId) -> Self {
        value.0
    }
}

impl From<usize> for OrgId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl FromStr for OrgId {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
    }
}

impl TryFrom<&str> for OrgId {
    type Error = <usize as FromStr>::Err;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(OrgId(value.parse()?))
    }
}

duplicate! {
    [
        number;
        [ u64 ];
        [ u32 ];
        [ u16 ];
        [ u8 ];
        [ i64 ];
        [ i32 ];
        [ i16 ];
        [ i8 ];
        [ isize ];
    ]
    impl From<OrgId> for number {
        fn from(value: OrgId) -> Self {
            value.0 as number
        }
    }
    impl From<number> for OrgId {
        fn from(value: number) -> Self {
            Self(value as usize)
        }
    }
}

impl From<&OrgId> for OrgId {
    fn from(value: &OrgId) -> Self {
        *value
    }
}
