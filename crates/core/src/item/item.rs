use serde::{Deserialize, Serialize};
use time::OffsetDateTime;
use typed_builder::TypedBuilder;

use super::{CoreItem, ItemId, ItemProperty, ItemReference};

#[derive(Clone, Debug, TypedBuilder, PartialEq, Eq, Serialize, Deserialize)]
pub struct Item {
    pub id: ItemId,

    #[builder(setter(into))]
    pub name: String,

    #[builder(default, setter(into, strip_option))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content_type: Option<String>,

    #[builder(default, setter(into, strip_option))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,

    #[builder(default, setter(strip_option))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<Vec<ItemProperty>>,

    #[builder(default, setter(strip_option))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parents: Option<Vec<CoreItem>>,

    #[builder(default, setter(strip_option))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub children: Option<Vec<CoreItem>>,

    #[builder(default, setter(strip_option))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub references: Option<Vec<ItemReference>>,

    #[builder(default, setter(strip_option))]
    #[serde(with = "time::serde::rfc3339::option")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub create_time: Option<OffsetDateTime>,

    #[builder(default, setter(strip_option))]
    #[serde(with = "time::serde::rfc3339::option")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub update_time: Option<OffsetDateTime>,
}
