use std::any::Any;

use anyhow::Result;
use async_trait::async_trait;
use silver_brain_core::StoreName;
use thiserror::Error;

// =================================================================
//  Store Error
// =================================================================

#[derive(Error, Debug, PartialEq, Eq)]
pub enum StoreError {
    #[error("Data path not a directory")]
    DataPathNotDirectory,

    #[error("Data path not writable")]
    DataPathNotWritable,
}

// =================================================================
//  Store
// =================================================================

#[async_trait]
pub trait Store<Connection>: Any + Send + Sync {
    async fn get_conn(&self, store_name: &StoreName) -> Result<Connection>;
}
