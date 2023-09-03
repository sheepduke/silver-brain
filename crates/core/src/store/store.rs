use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{ensure, Context, Result};
use async_trait::async_trait;
use sea_orm::Database;
use sea_orm::DatabaseConnection;
use thiserror::Error;

use crate::{Entry, EntryId, Link, LinkId};

// =================================================================
//  StoreName
// =================================================================

#[derive(Debug, PartialEq, Eq)]
pub struct StoreName(String);

impl From<&'static str> for StoreName {
    fn from(value: &'static str) -> Self {
        StoreName(value.into())
    }
}

impl From<String> for StoreName {
    fn from(value: String) -> Self {
        StoreName(value)
    }
}

impl From<StoreName> for String {
    fn from(value: StoreName) -> Self {
        value.0
    }
}

// =================================================================
//  StoreConnection
// =================================================================

pub trait StoreConnection {
    type Connection;
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
//  Store
// =================================================================

#[async_trait]
pub trait Store: StoreConnection + EntryRepo {
    async fn get_connection(&self, name: StoreName) -> Result<Self::Connection>;
}

// =================================================================
//  EntryRepo
// =================================================================

#[async_trait]
pub trait EntryRepo: StoreConnection {
    async fn create_entry(&self, conn: &Self::Connection) -> Result<EntryId>;

    async fn get_entry(&self, conn: &Self::Connection, id: EntryId) -> Result<Entry>;
}
