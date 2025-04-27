use typed_builder::TypedBuilder;

use crate::{RequestContext, ServiceResponse};

use super::{Item, ItemId};

pub trait ItemService {
    async fn get_item(
        &self,
        context: &RequestContext,
        id: &str,
        options: &ItemLoadOptions,
    ) -> ServiceResponse<Option<Item>>;

    async fn create_item(
        &self,
        context: &RequestContext,
        request: &CreateItemRequest,
    ) -> ServiceResponse<ItemId>;

    async fn update_item(
        &self,
        context: &RequestContext,
        request: &UpdateItemRequest,
    ) -> ServiceResponse<()>;

    async fn delete_item(&self, context: &RequestContext, id: &str) -> ServiceResponse<()>;

    async fn upsert_property(
        &self,
        context: &RequestContext,
        request: &UpsertPropertyRequest,
    ) -> ServiceResponse<()>;

    async fn delete_property(&self, context: &RequestContext, key: &str) -> ServiceResponse<()>;
}

#[derive(Debug, TypedBuilder)]
pub struct CreateItemRequest {
    #[builder(setter(into))]
    pub name: String,

    #[builder(setter(into))]
    pub content_type: String,

    #[builder(setter(into))]
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

#[derive(Default, TypedBuilder)]
pub struct ItemLoadOptions {
    #[builder(default)]
    pub load_content_type: bool,

    #[builder(default)]
    pub load_content: bool,

    #[builder(default)]
    pub load_properties: bool,

    #[builder(default)]
    pub load_parents: bool,

    #[builder(default)]
    pub load_children: bool,

    #[builder(default)]
    pub load_create_time: bool,

    #[builder(default)]
    pub load_update_time: bool,
}
