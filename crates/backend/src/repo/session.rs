use anyhow::Context;
use silver_brain_core::ServiceResponse;
use sqlx::{Sqlite, SqlitePool, Transaction};

// ============================================================
//  StoreSession
// ============================================================

pub trait StoreSession {
    async fn get_pool(&self, store_name: &str) -> ServiceResponse<SqlitePool>;

    async fn with_transaction<R>(
        &self,
        store_name: &str,
        fun: impl AsyncFnOnce(&mut Transaction<Sqlite>) -> ServiceResponse<R>,
    ) -> ServiceResponse<R>;
}

// ============================================================
//  SqliteStoreSession
// ============================================================

pub struct SqliteStoreSession {}

// ============================================================
//  InMemoryStoreSession
// ============================================================

pub struct InMemoryStoreSession {
    pool: SqlitePool,
}

impl InMemoryStoreSession {
    pub fn new() -> anyhow::Result<Self> {
        let pool = SqlitePool::connect_lazy("sqlite::memory:").context("Create in-memory pool")?;

        Ok(Self { pool })
    }
}

impl StoreSession for InMemoryStoreSession {
    async fn get_pool(&self, _store_name: &str) -> ServiceResponse<SqlitePool> {
        sqlx::migrate!()
            .run(&self.pool)
            .await
            .context("Run migrate")?;

        Ok(self.pool.clone())
    }

    async fn with_transaction<R>(
        &self,
        _store_name: &str,
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
