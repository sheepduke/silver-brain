use std::str::FromStr;

use derive_more::{AsRef, Deref, Display, Into};
use serde::{Deserialize, Serialize};
use svix_ksuid::{Ksuid, KsuidLike};

use crate::{ServiceError, util};

#[derive(
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Into,
    AsRef,
    Deref,
    Display,
    Serialize,
    Deserialize,
)]
pub struct ItemId(String);

#[allow(clippy::new_without_default)]
impl ItemId {
    /// Create a new ItemId from current timestamp.
    pub fn new() -> Self {
        let ksuid = Ksuid::new(None, None);
        ItemId(format!("i_{}", ksuid))
    }
}

impl FromStr for ItemId {
    type Err = ServiceError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("i_") {
            util::extract_ksuid(s)
                .map(|_| ItemId(s.to_string()))
                .ok_or(ServiceError::InvalidArgument(s.to_string()))
        } else {
            Err(ServiceError::InvalidArgument(s.to_string()))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use anyhow::Result;

    use super::*;

    #[test]
    fn new() {
        let item_id = ItemId::new();
        assert!(item_id.as_str().starts_with("i_"));
    }

    #[test]
    fn from_str() -> Result<()> {
        let id = format!("i_{}", Ksuid::new(None, None));
        let item_id: ItemId = id.parse()?;

        assert_eq!(item_id.as_str(), id);
        Ok(())
    }

    #[test]
    fn from_str_invalid() {
        let id = "invalid";
        let result = ItemId::from_str(id);

        assert!(matches!(result, Err(ServiceError::InvalidArgument(_))));
    }
}
