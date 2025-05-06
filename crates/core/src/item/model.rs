use std::collections::HashMap;
use std::str::FromStr;

use svix_ksuid::{Ksuid, KsuidLike};
use time::OffsetDateTime;
use typed_builder::TypedBuilder;

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

impl TryFrom<String> for ItemId {
    type Error = ServiceError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        ItemId::from_str(value.as_str())
    }
}

impl From<ItemId> for String {
    fn from(value: ItemId) -> Self {
        value.0.to_string()
    }
}

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

// ============================================================
//  Item
// ============================================================

#[derive(Clone, Debug, TypedBuilder, PartialEq, Eq)]
pub struct Item {
    pub id: ItemId,

    #[builder(setter(into))]
    pub name: String,

    #[builder(default, setter(into, strip_option))]
    pub content_type: Option<String>,

    #[builder(default, setter(into, strip_option))]
    pub content: Option<String>,

    #[builder(default, setter(strip_option))]
    pub parents: Option<Vec<CoreItem>>,

    #[builder(default, setter(strip_option))]
    pub children: Option<Vec<CoreItem>>,

    #[builder(default, setter(strip_option))]
    pub properties: Option<HashMap<String, String>>,

    #[builder(default, setter(strip_option))]
    pub create_time: Option<OffsetDateTime>,

    #[builder(default, setter(strip_option))]
    pub update_time: Option<OffsetDateTime>,
}

impl Item {
    pub fn new(id: impl Into<ItemId>, name: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            content_type: None,
            content: None,
            parents: None,
            children: None,
            properties: None,
            create_time: None,
            update_time: None,
        }
    }
}

// ============================================================
//  Core Item
// ============================================================

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoreItem {
    pub id: ItemId,
    pub name: String,
}

impl CoreItem {
    pub fn new(id: impl Into<ItemId>, name: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
        }
    }
}
