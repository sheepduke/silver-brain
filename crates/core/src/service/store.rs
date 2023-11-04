use anyhow::Result;
use async_trait::async_trait;

use crate::StoreName;

#[async_trait]
pub trait StoreService {
    async fn create_store(&self, name: &StoreName) -> Result<()>;

    async fn list_stores(&self) -> Result<Vec<StoreName>>;

    async fn delete_store(&self, name: &StoreName) -> Result<()>;
}
