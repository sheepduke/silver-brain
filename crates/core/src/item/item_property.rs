use serde::{Deserialize, Serialize};
use typed_builder::TypedBuilder;

#[derive(Debug, Clone, PartialEq, Eq, TypedBuilder, Serialize, Deserialize)]
pub struct ItemProperty {
    #[builder(setter(into))]
    pub key: String,

    #[builder(setter(into))]
    pub value: String,
}
