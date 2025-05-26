use silver_brain_backend::{SqlService, SqliteConnector};

#[derive(Debug)]
pub(crate) struct AppState {
    pub service: SqlService<SqliteConnector>,
}

impl AppState {
    pub fn new(service: SqlService<SqliteConnector>) -> Self {
        Self { service }
    }
}
