use crate::ServiceResponse;

use super::{Item, ItemId};

pub trait ItemService {
    fn create_item(request: CreateItemRequest) -> ServiceResponse<ItemId>;

    fn get_item(id: ItemId, options: ItemLoadOptions) -> ServiceResponse<Option<Item>>;

    fn update_item(request: UpdateItemRequest) -> ServiceResponse<()>;

    fn delete_item(id: ItemId) -> ServiceResponse<()>;

    fn upsert_property(request: UpsertPropertyRequest) -> ServiceResponse<()>;

    fn delete_property(key: &str) -> ServiceResponse<()>;
}

pub struct CreateItemRequest {
    pub name: String,
    pub content_type: String,
    pub content: String,
}

pub struct UpdateItemRequest {
    pub id: String,
    pub name: Option<String>,
    pub content_type: Option<String>,
    pub content: Option<String>,
}

pub struct UpsertPropertyRequest {
    pub item_id: String,
    pub key: String,
    pub value: String,
}

#[derive(Default)]
pub struct ItemLoadOptions {
    pub load_content_type: bool,

    pub load_content: bool,

    pub load_properties: bool,

    pub load_parents: bool,

    pub load_children: bool,

    pub load_create_time: bool,

    pub load_update_time: bool,
}

impl ItemLoadOptions {
    pub fn with_content_type(mut self) -> Self {
        self.load_content_type = true;
        self
    }

    pub fn with_content(mut self) -> Self {
        self.load_content = true;
        self
    }

    pub fn with_create_time(mut self) -> Self {
        self.load_create_time = true;
        self
    }

    pub fn with_update_time(mut self) -> Self {
        self.load_update_time = true;
        self
    }
}
