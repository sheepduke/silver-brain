use anyhow::Context;
use silver_brain_core::{ServiceResponse, service::RepoName};
use sqlx::{Sqlite, SqlitePool, Transaction};

// ============================================================
//  DatabaseConnector
// ============================================================

pub trait DatabaseConnector {
    async fn with_transaction<R>(
        &self,
        repo_name: &RepoName,
        fun: impl AsyncFnOnce(&mut Transaction<Sqlite>) -> ServiceResponse<R>,
    ) -> ServiceResponse<R>;
}

// ============================================================
//  SqliteConnector
// ============================================================

pub struct SqliteStoreSession {}

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
    async fn with_transaction<R>(
        &self,
        _repo_name: &RepoName,
        fun: impl AsyncFnOnce(&mut Transaction<Sqlite>) -> ServiceResponse<R>,
    ) -> ServiceResponse<R> {
        with_transaction(&self.pool, fun).await
    }
}

// ============================================================
//  Private Functions
// ============================================================

async fn with_transaction<R>(
    pool: &SqlitePool,
    fun: impl AsyncFnOnce(&mut Transaction<Sqlite>) -> ServiceResponse<R>,
) -> ServiceResponse<R> {
    sqlx::migrate!().run(pool).await.context("Run migrate")?;

    let mut transaction = pool.begin().await.context("Begin transaction")?;

    let result = fun(&mut transaction).await;

    if result.is_ok() {
        transaction.commit().await.context("Commit transaction")?;
    }

    result
}
