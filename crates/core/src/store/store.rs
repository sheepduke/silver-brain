use std::any::Any;

use anyhow::Result;
use async_trait::async_trait;
use thiserror::Error;

use crate::StoreName;

// =================================================================
//  Store Error
// =================================================================

#[derive(Error, Debug, PartialEq, Eq)]
pub enum StoreError {
    #[error("Data path not a directory")]
    DataPathNotDirectory,

    #[error("Data path not writable")]
    DataPathNotWritable,

    #[error("Invalid database name")]
    InvalidDatabaseName,
}

// =================================================================
//  StoreConnection
// =================================================================

#[async_trait]
pub trait Store<Connection>: Any + Send + Sync {
    async fn get_conn(&self, store_name: &StoreName) -> Result<Connection>;
}
