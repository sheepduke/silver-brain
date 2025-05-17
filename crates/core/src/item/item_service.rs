use typed_builder::TypedBuilder;

use crate::{RequestContext, ServiceResponse};

use super::{Item, ItemId};

pub trait ItemService {
    fn get_item(
        &self,
        context: &RequestContext,
        id: &str,
        options: &ItemLoadOptions,
    ) -> impl Future<Output = ServiceResponse<Option<Item>>> + Send;

    fn create_item(
        &self,
        context: &RequestContext,
        request: CreateItemRequest,
    ) -> impl Future<Output = ServiceResponse<ItemId>> + Send;

    fn update_item(
        &self,
        context: &RequestContext,
        request: UpdateItemRequest,
    ) -> impl Future<Output = ServiceResponse<()>> + Send;

    fn delete_item(
        &self,
        context: &RequestContext,
        id: &str,
    ) -> impl Future<Output = ServiceResponse<()>> + Send;

    fn upsert_item_property(
        &self,
        context: &RequestContext,
        request: UpsertItemPropertyRequest,
    ) -> impl Future<Output = ServiceResponse<()>> + Send;

    fn delete_item_property(
        &self,
        context: &RequestContext,
        item_id: &str,
        key: &str,
    ) -> impl Future<Output = ServiceResponse<()>> + Send;
}

#[derive(Debug, TypedBuilder)]
pub struct CreateItemRequest {
    #[builder(setter(into))]
    pub name: String,

    #[builder(default, setter(into, strip_option))]
    pub content_type: Option<String>,

    #[builder(default, setter(into, strip_option))]
    pub content: Option<String>,
}

#[derive(Debug, TypedBuilder)]
pub struct UpdateItemRequest {
    #[builder(setter(into))]
    pub id: String,

    #[builder(default, setter(into, strip_option))]
    pub name: Option<String>,

    #[builder(default, setter(into, strip_option))]
    pub content_type: Option<String>,

    #[builder(default, setter(into, strip_option))]
    pub content: Option<String>,
}

#[derive(Debug, TypedBuilder)]
pub struct UpsertItemPropertyRequest {
    #[builder(setter(into))]
    pub item_id: String,

    #[builder(setter(into))]
    pub key: String,

    #[builder(setter(into))]
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

impl ItemLoadOptions {
    pub fn core() -> Self {
        Self::builder().build()
    }

    pub fn all() -> Self {
        Self {
            load_content_type: true,
            load_content: true,
            load_properties: true,
            load_parents: true,
            load_children: true,
            load_create_time: true,
            load_update_time: true,
        }
    }
}
