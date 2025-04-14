use std::str::FromStr;

use svix_ksuid::{Ksuid, KsuidLike};

use crate::{ServiceError, util};

// ============================================================
//  ItemId
// ============================================================

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemId(String);

#[allow(clippy::new_without_default)]
impl ItemId {
    /// Create a new ItemId from current timestamp.
    pub fn new() -> Self {
        let ksuid = Ksuid::new(None, None);
        ItemId(format!("i_{}", ksuid))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl FromStr for ItemId {
    type Err = ServiceError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("i_") {
            util::extract_ksuid(s)
                .map(|_| ItemId(s.to_string()))
                .ok_or(ServiceError::InvalidId(s.to_string()))
        } else {
            Err(ServiceError::InvalidId(s.to_string()))
        }
    }
}

impl From<ItemId> for String {
    fn from(value: ItemId) -> Self {
        value.0.to_string()
    }
}

// ============================================================
//  Test
// ============================================================

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_new() {
        let item_id = ItemId::new();
        assert!(item_id.as_str().starts_with("i_"));
    }

    #[test]
    fn test_from_str() {
        let id = format!("i_{}", Ksuid::new(None, None));
        let item_id = ItemId::from_str(&id).expect("from_str not working");

        assert_eq!(item_id.as_str(), id);
    }

    #[test]
    fn test_from_str_invalid() {
        let id = "invalid";
        let result = ItemId::from_str(id);

        assert!(matches!(result, Err(ServiceError::InvalidId(_))));
    }
}
