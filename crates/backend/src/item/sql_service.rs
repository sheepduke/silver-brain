use std::sync::Arc;

use silver_brain_core::ServiceResponse;

use crate::DatabaseConnector;

pub struct SqlService<C>
where
    C: DatabaseConnector,
{
    pub(crate) connector: Arc<C>,
}

impl<C> SqlService<C>
where
    C: DatabaseConnector + Send + Sync,
{
    pub fn new(connector: Arc<C>) -> Self {
        Self { connector }
    }

    pub async fn test(&self) -> ServiceResponse<()> {
        println!("WHAT??");
        Ok(())
    }
}
