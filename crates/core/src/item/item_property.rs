use typed_builder::TypedBuilder;

#[derive(Debug, Clone, PartialEq, Eq, TypedBuilder)]
pub struct ItemProperty {
    #[builder(setter(into))]
    pub key: String,

    #[builder(setter(into))]
    pub value: String,
}
