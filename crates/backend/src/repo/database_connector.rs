use std::path::PathBuf;

use anyhow::Context;
use silver_brain_core::{ServiceResponse, service::RepoName};
use sqlx::{Sqlite, SqlitePool, Transaction};

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
}

impl SqliteConnector {
    pub fn new(root_path: impl Into<PathBuf>) -> Self {
        Self {
            root_path: root_path.into(),
        }
    }
}

impl DatabaseConnector for SqliteConnector {
    async fn with_transaction<T>(
        &self,
        repo_name: &RepoName,
        fun: impl AsyncFnOnce(&mut Transaction<Sqlite>) -> ServiceResponse<T>,
    ) -> ServiceResponse<T> {
        todo!()
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
