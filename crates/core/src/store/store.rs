use anyhow::{ensure, Context, Result};
use sea_orm::{Database, DatabaseConnection};
use std::fs;
use std::path::{Path, PathBuf};
use thiserror::Error;

pub struct StoreName(&'static str);

pub struct StoreConnection(DatabaseConnection);

impl From<DatabaseConnection> for StoreConnection {
    fn from(value: DatabaseConnection) -> Self {
        Self(value)
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum StoreError {
    #[error("Data path not a directory")]
    DataPathNotDirectory,

    #[error("Data path not writable")]
    DataPathNotWritable,

    #[error("Invalid database name")]
    InvalidDatabaseName,
}

#[derive(Debug)]
pub struct Store {
    data_path: PathBuf,
}

impl Store {
    pub fn new(data_path: &Path) -> Result<Self> {
        ensure!(data_path.is_dir(), StoreError::DataPathNotDirectory);

        ensure!(
            is_dir_writable(data_path).context("Failed to check directory permission")?,
            StoreError::DataPathNotWritable
        );

        Ok(Self {
            data_path: data_path.to_path_buf(),
        })
    }

    pub async fn connect(&self, db_name: StoreName) -> Result<StoreConnection> {
        let db_path = self.resolve_sqlite_path(db_name);
        let db_path_str = db_path.to_str().ok_or(StoreError::InvalidDatabaseName)?;
        let conn_str = format!("sqlite:{}", db_path_str);

        Ok(Database::connect(conn_str).await?.into())
    }

    fn resolve_sqlite_path(&self, db_name: StoreName) -> PathBuf {
        let mut result = self.data_path.clone();

        result.push(db_name.0);
        result.push("sqlite");

        result
    }
}

fn is_dir_writable(path: &Path) -> Result<bool> {
    Ok(!fs::metadata(path)?.permissions().readonly())
}
