use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::{ensure, Context, Result};
use async_trait::async_trait;
use sea_orm::{Database, DatabaseConnection};

use crate::{store::StoreError, StoreName};

use super::Store;

// ============================================================
//  SqlStore
// ============================================================

#[derive(Default, Debug)]
pub struct SqliteStore {
    data_path: PathBuf,
}

impl SqliteStore {
    pub fn new(data_path: &Path) -> Result<Self> {
        if data_path.exists() {
            ensure!(data_path.is_dir(), StoreError::DataPathNotDirectory);
            ensure!(
                Self::is_dir_writable(data_path).context("Failed to check directory permission")?,
                StoreError::DataPathNotWritable
            );
        } else {
            fs::create_dir_all(data_path)?;
        }

        Ok(Self {
            data_path: data_path.to_path_buf(),
        })
    }

    fn resolve_sqlite_path(&self, name: &StoreName) -> PathBuf {
        let mut result = self.data_path.clone();
        result.push(format!("{}.sqlite", name.0));
        result
    }

    fn is_dir_writable(path: &Path) -> Result<bool> {
        Ok(!fs::metadata(path)?.permissions().readonly())
    }
}

#[async_trait]
impl Store for SqliteStore {
    type Connection = DatabaseConnection;

    async fn get_conn(&self, store_name: &StoreName) -> Result<DatabaseConnection> {
        let db_path = self.resolve_sqlite_path(store_name);
        let db_path_str = db_path.to_str().ok_or(StoreError::InvalidDatabaseName)?;
        let conn_str = format!("sqlite:{}", db_path_str);

        Ok(Database::connect(conn_str).await?.into())
    }
}

#[cfg(test)]
mod tests {
    use std::{env, fs::File};

    use super::*;

    #[tokio::test]
    async fn new_get_conn() {
        let mut data_path = env::temp_dir();
        data_path.push("silver-brain");

        // Ensure the creation succeeds.
        let result = SqliteStore::new(&data_path);
        assert!(result.is_ok());

        // Create the test sqlite file if not exists.
        let mut sqlite_file = data_path.clone();
        sqlite_file.push("test.sqlite");
        let _ = File::create(&sqlite_file).expect("Create test.sqlite file");

        // Ensure the database can be connected.
        let store = result.unwrap();
        let store_name = StoreName("test".to_string());
        let conn = store.get_conn(&store_name).await;

        assert!(conn.is_ok());
    }
}
