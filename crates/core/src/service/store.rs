use async_trait::async_trait;

use crate::service::StoreName;

use super::ServiceResult;

#[async_trait]
pub trait StoreService {
    async fn create_store(&self, name: &StoreName) -> ServiceResult<()>;

    async fn list_stores(&self) -> ServiceResult<Vec<StoreName>>;

    async fn delete_store(&self, name: &StoreName) -> ServiceResult<()>;
}
