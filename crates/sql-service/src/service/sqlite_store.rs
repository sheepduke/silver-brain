use std::{
    fs::{self, File},
    path::{Path, PathBuf},
};

use async_trait::async_trait;
use migration::Migrator;
use sea_orm::{Database, DatabaseConnection};

use tracing::debug;

use silver_brain_core::service::{ServiceError, ServiceResult, StoreName, StoreService};

// ============================================================
//  SqlStore
// ============================================================

#[derive(Debug)]
pub struct SqliteStore {
    pub data_path: PathBuf,
}

impl SqliteStore {
    pub fn new(data_path: PathBuf) -> ServiceResult<Self> {
        if data_path.exists() {
            if !data_path.is_dir() {
                panic!("Data path is not a directory");
            }

            if !Self::is_dir_writable(&data_path).expect("Failed to check directory permission") {
                panic!("Data path is not writable");
            }
        } else {
            debug!("Creating data directory");
            fs::create_dir_all(&data_path)?;
        }

        Ok(Self { data_path })
    }

    pub async fn get_conn(&self, store_name: &StoreName) -> ServiceResult<DatabaseConnection> {
        let sqlite_path = self.resolve_sqlite_path(store_name);

        let sqlite_file_path_str = sqlite_path.to_str().ok_or(ServiceError::InvalidStoreName)?;
        let conn_str = format!("sqlite:{}", &sqlite_file_path_str);
        let conn = Database::connect(&conn_str).await?;

        Ok(conn)
    }

    fn is_dir_writable(path: &Path) -> ServiceResult<bool> {
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
    async fn create_store(&self, store_name: &StoreName) -> ServiceResult<()> {
        let store_path = self.resolve_store_path(store_name);

        if !store_path.exists() {
            fs::create_dir_all(&store_path)?;
            fs::create_dir(&store_path.join("attachments"))?;

            // Create SQLite file.
            let sqlite_file_path = self.resolve_sqlite_path(store_name);
            File::create(&sqlite_file_path)?;

            // Migrate SQLite database.
            let conn = self.get_conn(store_name).await?;
            Migrator::run(&conn).await?;
        }

        Ok(())
    }

    async fn list_stores(&self) -> ServiceResult<Vec<StoreName>> {
        let mut path = self.data_path.clone();
        path.push("store");

        let store_names = fs::read_dir(path)?
            .map(|r| {
                r.map(|path| {
                    path.file_name()
                        .into_string()
                        .map(|name| StoreName(name))
                        .map_err(|_| ServiceError::Other(anyhow::anyhow!("Invalid file name")))
                })?
            })
            .collect::<Result<Vec<StoreName>, _>>()?;

        Ok(store_names)
    }

    async fn delete_store(&self, name: &StoreName) -> ServiceResult<()> {
        let path = self.resolve_store_path(name);

        if path.try_exists()? {
            fs::remove_dir_all(&path)?;
        }

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
