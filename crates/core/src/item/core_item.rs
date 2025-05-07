use typed_builder::TypedBuilder;

use super::ItemId;

#[derive(Clone, Debug, PartialEq, Eq, TypedBuilder)]
pub struct CoreItem {
    pub id: ItemId,

    #[builder(setter(into))]
    pub name: String,
}
