use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::RwLock,
};

use anyhow::Context;
use silver_brain_core::{ServiceError, ServiceResponse, service::RepoName};
use sqlx::{Sqlite, SqlitePool, Transaction};

use super::util::ToServiceResponse;

// ============================================================
//  DatabaseConnector
// ============================================================

pub trait DatabaseConnector {
    async fn with_transaction<T>(
        &self,
        repo_name: &RepoName,
        fun: impl AsyncFnOnce(&mut Transaction<Sqlite>) -> ServiceResponse<T>,
    ) -> ServiceResponse<T>;
}

// ============================================================
//  SqliteConnector
// ============================================================

pub struct SqliteConnector {
    root_path: PathBuf,
    pools: RwLock<HashMap<String, SqlitePool>>,
}

impl SqliteConnector {
    pub fn new(root_path: impl Into<PathBuf>) -> Self {
        Self {
            root_path: root_path.into(),
            pools: RwLock::new(HashMap::new()),
        }
    }

    async fn get_or_create_pool(&self, repo_name: &RepoName) -> ServiceResponse<SqlitePool> {
        let pools = self.pools.read().map_err(|err| {
            ServiceError::Internal("Failed to acquire database read lock".to_string())
        })?;

        if pools.contains_key(repo_name.as_str()) {
            Ok(pools.get(repo_name.as_str()).unwrap().clone())
        } else {
            let path = self.resolve_sqlite_path(repo_name)?;

            let url = format!(
                "sqlite:{}",
                path.to_str().ok_or(ServiceError::InvalidArgument(
                    "Invalid repo name".to_string()
                ),)?
            );

            let pool = SqlitePool::connect_lazy(&url).to_service_response()?;
            sqlx::migrate!().run(&pool).await.context("Run migrate")?;

            drop(pools);

            let mut pools = self.pools.write().map_err(|err| {
                ServiceError::Internal(format!(
                    "Failed to acquire database write lock\n{}",
                    err.to_string()
                ))
            })?;

            pools.insert(repo_name.to_string(), pool.clone());

            Ok(pool)
        }
    }

    fn resolve_sqlite_path(&self, repo_name: &RepoName) -> ServiceResponse<PathBuf> {
        let mut path = self.root_path.clone();
        path.push(repo_name.as_str());
        path.push("data.sqlite");

        if let Ok(true) = path.try_exists() {
            Ok(path)
        } else {
            Err(ServiceError::InvalidArgument(
                "Database does not exist".to_string(),
            ))
        }
    }
}

impl DatabaseConnector for SqliteConnector {
    async fn with_transaction<T>(
        &self,
        repo_name: &RepoName,
        fun: impl AsyncFnOnce(&mut Transaction<Sqlite>) -> ServiceResponse<T>,
    ) -> ServiceResponse<T> {
        let pool = self.get_or_create_pool(repo_name).await?;

        let mut transaction = pool.begin().await.to_service_response()?;

        fun(&mut transaction).await
    }
}

#[cfg(test)]
mod tests {
    use std::{
        env,
        fs::{self, File},
        path::{Path, PathBuf},
    };

    use anyhow::{Context, Result};
    use silver_brain_core::*;
    use svix_ksuid::{Ksuid, KsuidLike};

    use crate::repo;

    use super::{DatabaseConnector, SqliteConnector};

    #[tokio::test]
    async fn with_transaction() -> Result<()> {
        let dir_id = Ksuid::new(None, None).to_string();

        let mut root_path = env::temp_dir();
        root_path.push(dir_id);

        let sqlite_dir_path = root_path.join("main");
        fs::create_dir_all(&sqlite_dir_path).unwrap();

        let sqlite_file_path = sqlite_dir_path.join("data.sqlite");
        File::create(sqlite_file_path).unwrap();

        let connector = SqliteConnector::new(&root_path);
        let repo_name = "main".parse().unwrap();

        connector
            .with_transaction(&repo_name, async |tx| {
                let item_opt =
                    repo::item::get(tx, &ItemId::new(), &ItemLoadOptions::core()).await?;

                assert!(item_opt.is_none());

                Ok(())
            })
            .await?;

        fs::remove_dir_all(root_path.to_str().unwrap()).unwrap();

        Ok(())
    }
}

// ============================================================
//  InMemorySqliteConnector
// ============================================================

pub struct InMemorySqliteConnector {
    pool: SqlitePool,
}

impl InMemorySqliteConnector {
    pub fn new() -> anyhow::Result<Self> {
        let pool = SqlitePool::connect_lazy("sqlite::memory:").context("Create in-memory pool")?;

        Ok(Self { pool })
    }
}

impl DatabaseConnector for InMemorySqliteConnector {
    async fn with_transaction<T>(
        &self,
        _repo_name: &RepoName,
        fun: impl AsyncFnOnce(&mut Transaction<Sqlite>) -> ServiceResponse<T>,
    ) -> ServiceResponse<T> {
        sqlx::migrate!()
            .run(&self.pool)
            .await
            .context("Run migrate")?;

        let mut transaction = self.pool.begin().await.context("Begin transaction")?;

        let result = fun(&mut transaction).await;

        if result.is_ok() {
            transaction.commit().await.context("Commit transaction")?;
        }

        result
    }
}
