use std::{path::PathBuf, sync::Arc};

use silver_brain_sql_service::{SqlEntryService, SqliteStore};
use typed_builder::TypedBuilder;

#[derive(Debug)]
pub struct ServerState {
    pub store: Arc<SqliteStore>,
    pub entry_service: SqlEntryService,
}

impl ServerState {
    pub fn new(args: ServerStateArgs) -> Self {
        let store =
            Arc::new(SqliteStore::new(args.data_path).expect("Failed to create SQLite store."));

        let entry_service = SqlEntryService::new(store.clone());

        Self {
            store,
            entry_service,
        }
    }
}

#[derive(Clone, TypedBuilder, Debug)]
pub struct ServerStateArgs {
    data_path: PathBuf,
}
