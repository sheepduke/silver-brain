use typed_builder::TypedBuilder;

use crate::*;

pub trait ItemReferenceService {
    async fn create(
        context: &RequestContext,
        request: CreateItemReferenceRequest,
    ) -> ServiceResponse<ItemReferenceId>;

    async fn update(
        context: &RequestContext,
        request: UpdateItemReferenceRequest,
    ) -> ServiceResponse<()>;

    async fn delete(context: &RequestContext, id: &ItemReferenceId) -> ServiceResponse<()>;
}

#[derive(Debug, TypedBuilder)]
pub struct CreateItemReferenceRequest {
    #[builder(setter(into))]
    pub source: String,

    #[builder(setter(into))]
    pub target: String,

    #[builder(setter(into))]
    pub annotation: String,
}

#[derive(Debug, TypedBuilder)]
pub struct UpdateItemReferenceRequest {
    #[builder(setter(into))]
    pub source: String,

    #[builder(setter(into))]
    pub target: String,

    #[builder(setter(into))]
    pub annotation: Option<String>,
}
