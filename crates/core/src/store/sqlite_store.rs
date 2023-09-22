use std::{
    fs::{self, File},
    path::{Path, PathBuf},
};

use anyhow::{ensure, Context, Result};
use async_trait::async_trait;
use migration::Migrator;
use sea_orm::{Database, DatabaseConnection};
use typed_builder::TypedBuilder;

use crate::{store::StoreError, StoreName};

use super::Store;

// ============================================================
//  SqlStore
// ============================================================

#[derive(Clone, PartialEq, Eq, TypedBuilder, Debug)]
pub struct SqliteStoreOptions {
    #[builder(default = true)]
    pub auto_create: bool,

    #[builder(default = true)]
    pub auto_migrate: bool,
}

impl Default for SqliteStoreOptions {
    fn default() -> Self {
        Self {
            auto_create: true,
            auto_migrate: true,
        }
    }
}

#[derive(Debug)]
pub struct SqliteStore {
    pub data_path: PathBuf,

    pub options: SqliteStoreOptions,
}

impl SqliteStore {
    pub fn new(data_path: PathBuf, options: SqliteStoreOptions) -> Result<Self> {
        if data_path.exists() {
            ensure!(data_path.is_dir(), StoreError::DataPathNotDirectory);
            ensure!(
                Self::is_dir_writable(&data_path)
                    .context("Failed to check directory permission")?,
                StoreError::DataPathNotWritable
            );
        } else {
            fs::create_dir_all(&data_path)?;
        }

        Ok(Self { data_path, options })
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
impl Store<DatabaseConnection> for SqliteStore {
    async fn get_conn(&self, store_name: &StoreName) -> Result<DatabaseConnection> {
        let db_path = self.resolve_sqlite_path(store_name);
        let db_path_str = db_path.to_str().ok_or(StoreError::InvalidDatabaseName)?;
        let conn_str = format!("sqlite:{}", db_path_str);

        let conn;

        if db_path.try_exists()? {
            conn = Database::connect(conn_str).await?;
        } else {
            if self.options.auto_create {
                File::create(&db_path)?;
            }

            conn = Database::connect(conn_str).await?;

            if self.options.auto_migrate {
                Migrator::run(&conn).await?;
            }
        }

        Ok(conn)
    }
}

#[cfg(test)]
mod tests {
    use crate::store::tests::store::new_sqlite_store;

    use super::*;

    #[tokio::test]
    async fn get_conn() {
        // Ensure the database can be connected.
        let store = new_sqlite_store();
        let store_name = StoreName("test".to_string());
        let conn = store.get_conn(&store_name).await;

        assert!(conn.is_ok());
    }
}
