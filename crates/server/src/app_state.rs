use silver_brain_backend::{SqlService, SqliteConnector};

pub struct AppState {
    pub service: SqlService<SqliteConnector>,
}

impl AppState {
    pub fn new(service: SqlService<SqliteConnector>) -> Self {
        Self { service }
    }
}
