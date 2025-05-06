use crate::{RequestContext, ServiceResponse};

pub trait ItemLinkService {
    async fn create_link(
        &self,
        context: &RequestContext,
        request: &CreateItemLinkRequest,
    ) -> ServiceResponse<()>;

    async fn delete_link(
        &self,
        context: &RequestContext,
        request: &DeleteItemLinkRequest,
    ) -> ServiceResponse<()>;
}

pub struct CreateItemLinkRequest {
    pub parent: String,
    pub child: String,
}

pub struct DeleteItemLinkRequest {
    pub parent: String,
    pub child: String,
}
