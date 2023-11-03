use anyhow::Result;
use async_trait::async_trait;

use crate::RequestContext;

#[async_trait]
pub trait StoreService {
    async fn create_store(&self, context: &RequestContext, name: String) -> Result<()>;

    async fn list_stores(&self, context: &RequestContext) -> Result<Vec<String>>;

    async fn delete_store(&self, context: &RequestContext, name: &str) -> Result<()>;
}
