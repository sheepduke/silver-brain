use crate::ServiceResponse;

use super::ItemId;

pub trait LinkService {
    fn create_link(parent: ItemId, child: ItemId) -> ServiceResponse<()>;

    fn delete_link(parent: ItemId, child: ItemId) -> ServiceResponse<()>;
}
