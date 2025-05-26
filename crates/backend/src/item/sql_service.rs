use std::sync::Arc;

use crate::DatabaseConnector;

#[derive(Debug)]
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
}
