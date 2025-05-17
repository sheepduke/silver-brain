use serde::{Deserialize, Serialize};
use time::OffsetDateTime;
use typed_builder::TypedBuilder;

use super::{ItemId, ItemReferenceId};

#[derive(Clone, Debug, PartialEq, Eq, TypedBuilder, Serialize, Deserialize)]
pub struct ItemReference {
    pub id: ItemReferenceId,

    pub source: ItemId,

    pub target: ItemId,

    #[builder(setter(into))]
    pub annotation: String,

    #[serde(with = "time::serde::rfc3339")]
    pub create_time: OffsetDateTime,

    #[serde(with = "time::serde::rfc3339")]
    pub update_time: OffsetDateTime,
}
