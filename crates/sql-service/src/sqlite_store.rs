use std::{
    fs::{self, DirEntry, File},
    path::{Path, PathBuf},
};

use anyhow::{ensure, Context, Result};
use async_trait::async_trait;
use migration::Migrator;
use sea_orm::{Database, DatabaseConnection};

use thiserror::Error;
use tracing::{debug, info, instrument};
use typed_builder::TypedBuilder;

use silver_brain_core::{RequestContext, ServiceClientError, StoreName, StoreService};

#[derive(Error, Debug, PartialEq, Eq)]
pub enum StoreError {
    #[error("Data path not a directory")]
    DataPathNotDirectory,

    #[error("Data path not writable")]
    DataPathNotWritable,
}

// ============================================================
//  SqlStore
// ============================================================

#[derive(Debug)]
pub struct SqliteStore {
    pub data_path: PathBuf,
}

impl SqliteStore {
    pub fn new(data_path: PathBuf) -> Result<Self> {
        if data_path.exists() {
            ensure!(data_path.is_dir(), StoreError::DataPathNotDirectory);
            ensure!(
                Self::is_dir_writable(&data_path)
                    .context("Failed to check directory permission")?,
                StoreError::DataPathNotWritable
            );
        } else {
            debug!("Creating data directory");
            fs::create_dir_all(&data_path)?;
        }

        Ok(Self { data_path })
    }

    pub async fn get_conn(&self, store_name: &StoreName) -> Result<DatabaseConnection> {
        let sqlite_path = self.resolve_sqlite_path(store_name);

        let sqlite_file_path_str = sqlite_path
            .to_str()
            .ok_or(ServiceClientError::InvalidStoreName(store_name.0.clone()))?;
        let conn_str = format!("sqlite:{}", &sqlite_file_path_str);
        let conn = Database::connect(&conn_str).await?;

        Ok(conn)
    }

    fn is_dir_writable(path: &Path) -> Result<bool> {
        Ok(!fs::metadata(path)?.permissions().readonly())
    }

    fn resolve_store_path(&self, StoreName(name): &StoreName) -> PathBuf {
        let mut path = self.data_path.clone();
        path.push("store");
        path.push(name);
        path
    }

    fn resolve_sqlite_path(&self, store_name: &StoreName) -> PathBuf {
        let mut path = self.resolve_store_path(store_name);
        path.push("data.sqlite");
        path
    }
}

#[async_trait]
impl StoreService for SqliteStore {
    async fn create_store(&self, store_name: &StoreName) -> Result<()> {
        let store_path = self.resolve_store_path(store_name);

        if !store_path.exists() {
            fs::create_dir_all(&store_path)?;
            fs::create_dir(&store_path.join("attachments"));

            // Create SQLite file.
            let sqlite_file_path = self.resolve_sqlite_path(store_name);
            File::create(&sqlite_file_path);

            // Migrate SQLite database.
            let conn = self.get_conn(store_name).await?;
            Migrator::run(&conn).await?;
        }

        Ok(())
    }

    async fn list_stores(&self) -> Result<Vec<StoreName>> {
        fs::read_dir(&self.data_path)?
            .map(|r| {
                r.map(|path| {
                    path.file_name()
                        .into_string()
                        .map(|name| StoreName(name))
                        .map_err(|_| anyhow::anyhow!("Invalid file name"))
                })
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
    }

    async fn delete_store(&self, name: &StoreName) -> Result<()> {
        let path = self.resolve_store_path(name);
        fs::remove_dir_all(&path)?;
        Ok(())
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    use std::env;
    use std::sync::Once;

    use lazy_static::lazy_static;
    use svix_ksuid::{Ksuid, KsuidLike};

    impl Drop for SqliteStore {
        fn drop(&mut self) {
            fs::remove_dir_all(&self.data_path).unwrap()
        }
    }

    lazy_static! {
        static ref INIT: Once = Once::new();
    }

    pub fn new_sqlite_store() -> SqliteStore {
        INIT.call_once(|| {
            tracing_subscriber::fmt()
                .with_max_level(tracing::Level::DEBUG)
                .with_test_writer()
                .init();
        });

        let mut path = env::temp_dir();
        path.push("silver-brain-tests");
        path.push(Ksuid::new(None, None).to_string());

        SqliteStore::new(path, SqliteStoreOptions::default()).unwrap()
    }

    #[tokio::test]
    async fn get_conn() {
        // Ensure the database can be connected.
        let store = new_sqlite_store();
        let store_name = StoreName("test".to_string());
        let conn = store.get_conn(&store_name).await;

        assert!(conn.is_ok());
    }
}
