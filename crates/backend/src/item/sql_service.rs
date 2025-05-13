use std::sync::Arc;

use crate::DatabaseConnector;

pub struct SqlService<C>
where
    C: DatabaseConnector,
{
    pub(crate) connector: Arc<C>,
}

impl<C> SqlService<C>
where
    C: DatabaseConnector,
{
    pub fn new(connector: Arc<C>) -> Self {
        Self { connector }
    }
}
