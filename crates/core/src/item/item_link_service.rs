use crate::ServiceResponse;

pub trait ItemLinkService {
    fn create_link(request: CreateItemLinkRequest) -> ServiceResponse<()>;

    fn delete_link(request: DeleteItemLinkRequest) -> ServiceResponse<()>;
}

pub struct CreateItemLinkRequest {
    pub parent: String,
    pub child: String,
}

pub struct DeleteItemLinkRequest {
    pub parent: String,
    pub child: String,
}
