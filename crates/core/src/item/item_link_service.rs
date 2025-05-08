use crate::{RequestContext, ServiceResponse};

pub trait ItemLinkService {
    async fn create_link(
        &self,
        context: &RequestContext,
        parent: &str,
        child: &str,
    ) -> ServiceResponse<()>;

    async fn delete_link(
        &self,
        context: &RequestContext,
        parent: &str,
        child: &str,
    ) -> ServiceResponse<()>;
}
