use typed_builder::TypedBuilder;

use crate::*;

pub trait ItemReferenceService {
    async fn get_source_references(
        &self,
        context: &RequestContext,
        source: &str,
    ) -> ServiceResponse<Vec<ItemReference>>;

    async fn get_target_references(
        &self,
        context: &RequestContext,
        target: &str,
    ) -> ServiceResponse<Vec<ItemReference>>;

    async fn create_reference(
        &self,
        context: &RequestContext,
        request: CreateItemReferenceRequest,
    ) -> ServiceResponse<ItemReferenceId>;

    async fn update_reference(
        &self,
        context: &RequestContext,
        request: UpdateItemReferenceRequest,
    ) -> ServiceResponse<()>;

    async fn delete_reference(&self, context: &RequestContext, id: &str) -> ServiceResponse<()>;
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
    pub id: String,

    #[builder(setter(into))]
    pub annotation: Option<String>,
}
