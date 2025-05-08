use time::OffsetDateTime;
use typed_builder::TypedBuilder;

use super::{CoreItem, ItemId, ItemProperty};

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
    pub properties: Option<Vec<ItemProperty>>,

    #[builder(default, setter(strip_option))]
    pub create_time: Option<OffsetDateTime>,

    #[builder(default, setter(strip_option))]
    pub update_time: Option<OffsetDateTime>,
}
