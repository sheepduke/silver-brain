use std::str::FromStr;

use derive_more::{AsRef, Debug, Deref, Display};
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
    AsRef,
    Deref,
    Display,
    Serialize,
    Deserialize,
)]
pub struct ItemReferenceId(String);

#[allow(clippy::new_without_default)]
impl ItemReferenceId {
    /// Create a new ItemId from current timestamp.
    pub fn new() -> Self {
        let ksuid = Ksuid::new(None, None);
        ItemReferenceId(format!("r_{}", ksuid))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl FromStr for ItemReferenceId {
    type Err = ServiceError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("r_") {
            util::extract_ksuid(s)
                .map(|_| ItemReferenceId(s.to_string()))
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
    use svix_ksuid::{Ksuid, KsuidLike};

    use crate::ServiceError;

    use super::ItemReferenceId;

    #[test]
    fn new() {
        let reference_id = ItemReferenceId::new();
        assert!(reference_id.as_str().starts_with("r_"));
    }

    #[test]
    fn from_str() -> Result<()> {
        let id: String = format!("r_{}", Ksuid::new(None, None));
        let reference_id: ItemReferenceId = id.parse()?;

        assert_eq!(reference_id.as_str(), id);

        Ok(())
    }

    #[test]
    fn from_str_invalid() -> Result<()> {
        let id = "invalid";
        let result = ItemReferenceId::from_str(id);

        assert!(matches!(result, Err(ServiceError::InvalidArgument(_))));

        Ok(())
    }
}
