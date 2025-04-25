use crate::ServiceResponse;

pub trait ItemLinkService {
    fn create_link(request: CreateItemLinkRequest) -> ServiceResponse<()>;

    fn delete_link(request: DeleteItemLinkRequest) -> ServiceResponse<()>;
}

pub struct CreateItemLinkRequest {
    parent: String,
    child: String,
}

pub struct DeleteItemLinkRequest {
    parent: String,
    child: String,
}
