use serde::{Deserialize, Serialize};
use time::OffsetDateTime;
use typed_builder::TypedBuilder;

use super::{CoreItem, ItemReferenceId};

#[derive(Clone, Debug, PartialEq, Eq, TypedBuilder, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ItemReference {
    pub id: ItemReferenceId,

    pub source: CoreItem,

    pub target: CoreItem,

    #[builder(setter(into))]
    pub annotation: String,

    #[serde(with = "time::serde::rfc3339")]
    pub create_time: OffsetDateTime,

    #[serde(with = "time::serde::rfc3339")]
    pub update_time: OffsetDateTime,
}
