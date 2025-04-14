use crate::ServiceResponse;

use super::{Item, ItemId};

pub trait ItemService {
    fn create_item(request: CreateItemRequest) -> ServiceResponse<()>;

    fn get_item(id: ItemId, options: ItemLoadOptions) -> ServiceResponse<Option<Item>>;

    fn update_item(id: ItemId, request: UpdateItemRequest) -> ServiceResponse<()>;

    fn delete_item(id: ItemId) -> ServiceResponse<()>;

    fn upsert_property(key: &str, value: &str) -> ServiceResponse<()>;

    fn delete_property(key: &str) -> ServiceResponse<()>;
}

pub struct CreateItemRequest {
    pub id: String,
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

pub struct ItemLoadOptions {
    pub load_content_type: bool,
    pub load_content: bool,
    pub load_properties: bool,
    pub load_parents: bool,
    pub load_children: bool,
    pub load_create_time: bool,
    pub load_update_time: bool,
}
