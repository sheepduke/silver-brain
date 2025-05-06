use std::sync::Arc;

use crate::DatabaseConnector;

pub struct SqlService<M>
where
    M: DatabaseConnector,
{
    pub(crate) connector: Arc<M>,
}

impl<M> SqlService<M>
where
    M: DatabaseConnector,
{
    pub fn new(session: Arc<M>) -> Self {
        Self { connector: session }
    }
}
