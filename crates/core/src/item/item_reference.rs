use time::OffsetDateTime;
use typed_builder::TypedBuilder;

use super::{ItemId, ItemReferenceId};

#[derive(Debug, PartialEq, Eq, TypedBuilder)]
pub struct ItemReference {
    pub id: ItemReferenceId,

    pub source: ItemId,

    pub target: ItemId,

    #[builder(setter(into))]
    pub annotation: String,

    pub create_time: OffsetDateTime,

    pub update_time: OffsetDateTime,
}
