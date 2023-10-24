use std::{
    fs::{self, File},
    path::{Path, PathBuf},
};

use anyhow::{ensure, Context, Result};
use async_trait::async_trait;
use migration::Migrator;
use sea_orm::{Database, DatabaseConnection};
use silver_brain_core::StoreName;
use typed_builder::TypedBuilder;

use crate::store::StoreError;

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

    fn resolve_sqlite_path(&self, StoreName(name): &StoreName) -> PathBuf {
        let mut result = self.data_path.clone();
        result.push(format!("{}.sqlite", name));
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
pub mod tests {
    use super::*;

    use std::env;
    use std::sync::Once;

    use lazy_static::lazy_static;
    use svix_ksuid::{Ksuid, KsuidLike};

    use crate::store::{SqliteStore, SqliteStoreOptions};

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
