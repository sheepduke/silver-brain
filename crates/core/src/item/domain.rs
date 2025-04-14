use std::iter::Map;

use time::OffsetDateTime;

use super::ItemId;

// ============================================================
//  Item
// ============================================================

#[derive(Clone, Debug)]
pub struct Item {
    pub id: ItemId,
    pub name: String,
    pub content_type: Option<String>,
    pub content: Option<String>,
    pub parents: Option<Vec<CoreItem>>,
    pub children: Option<Vec<CoreItem>>,
    pub properties: Option<Map<String, String>>,
    pub create_time: Option<OffsetDateTime>,
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

#[derive(Clone, Debug)]
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
