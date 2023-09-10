use std::any::Any;
use std::sync::Arc;

use anyhow::Result;
use async_trait::async_trait;
use thiserror::Error;

// =================================================================
//  StoreName
// =================================================================

#[derive(Debug, PartialEq, Eq)]
pub struct StoreName(pub String);

impl<T> From<T> for StoreName
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

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

pub struct StoreConnection(pub Arc<dyn Any + Send + Sync>);

// =================================================================
//  DataSource
// =================================================================

pub trait DataSource {
    type Connection;
}

// =================================================================
//  Store
// =================================================================

#[async_trait]
pub trait Store<D: DataSource> {
    type Connection;

    async fn get_conn(&self, name: StoreName) -> Result<StoreConnection>;
}

// ----------------------------------------------------------------------
//  DataAccess
// ----------------------------------------------------------------------

pub trait DataAccess<D: DataSource> {
    type Id;

    type Out;

    type LoadOption;

    fn get(conn: StoreConnection, id: Self::Id, options: Self::LoadOption) -> Self::Out;
}
