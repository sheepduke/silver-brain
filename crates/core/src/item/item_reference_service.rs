use serde::Deserialize;
use typed_builder::TypedBuilder;

use crate::*;

pub trait ItemReferenceService {
    fn get_source_references(
        &self,
        context: &RequestContext,
        source: &str,
    ) -> impl Future<Output = ServiceResponse<Vec<ItemReference>>> + Send;

    fn get_target_references(
        &self,
        context: &RequestContext,
        target: &str,
    ) -> impl Future<Output = ServiceResponse<Vec<ItemReference>>> + Send;

    fn create_reference(
        &self,
        context: &RequestContext,
        request: CreateItemReferenceRequest,
    ) -> impl Future<Output = ServiceResponse<ItemReferenceId>> + Send;

    fn update_reference(
        &self,
        context: &RequestContext,
        request: UpdateItemReferenceRequest,
    ) -> impl Future<Output = ServiceResponse<()>> + Send;

    fn delete_reference(
        &self,
        context: &RequestContext,
        id: &str,
    ) -> impl Future<Output = ServiceResponse<()>> + Send;
}

#[derive(Debug, TypedBuilder, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CreateItemReferenceRequest {
    #[builder(setter(into))]
    pub source: String,

    #[builder(setter(into))]
    pub target: String,

    #[builder(setter(into))]
    pub annotation: String,
}

#[derive(Debug, TypedBuilder, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UpdateItemReferenceRequest {
    #[builder(setter(into))]
    pub id: String,

    #[builder(setter(into))]
    pub annotation: Option<String>,
}
