use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::{ensure, Context, Result};
use async_trait::async_trait;
use sea_orm::{Database, DatabaseConnection};

use crate::{store::StoreError, Entry, EntryLoadOption, StoreName};

use super::Store;

// ============================================================
//  SqlStore
// ============================================================

#[derive(Default, Debug)]
pub struct SqlStore {
    data_path: PathBuf,
}

impl SqlStore {
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

        result.push::<String>(name.0.clone());
        result.push("sqlite");

        result
    }

    fn is_dir_writable(path: &Path) -> Result<bool> {
        Ok(!fs::metadata(path)?.permissions().readonly())
    }
}

#[async_trait]
impl Store for SqlStore {
    type Connection = DatabaseConnection;

    async fn get_conn(&self, store_name: &StoreName) -> Result<DatabaseConnection> {
        let db_path = self.resolve_sqlite_path(store_name);
        let db_path_str = db_path.to_str().ok_or(StoreError::InvalidDatabaseName)?;
        let conn_str = format!("sqlite:{}", db_path_str);

        Ok(Database::connect(conn_str).await?.into())
    }
}
