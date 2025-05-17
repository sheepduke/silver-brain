use crate::{RequestContext, ServiceResponse};

pub trait ItemLinkService {
    fn create_link(
        &self,
        context: &RequestContext,
        parent: &str,
        child: &str,
    ) -> impl Future<Output = ServiceResponse<()>> + Send;

    fn delete_link(
        &self,
        context: &RequestContext,
        parent: &str,
        child: &str,
    ) -> impl Future<Output = ServiceResponse<()>> + Send;
}
