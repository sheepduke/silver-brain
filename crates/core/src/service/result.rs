// ============================================================
//  Error
// ============================================================

use std::backtrace::Backtrace;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ServiceClientError {
    /// The provided ID is not found.
    #[error("Resource with ID `{0}` not found")]
    NotFound(String),

    #[error("Invalid arguments `{0}`")]
    BadArguments(String),

    #[error("Invalid store name `{0}`")]
    InvalidStoreName(String),

    #[error("Invalid file path `{0}` for attachment")]
    InvalidAttachmentFilePath(String),
    // FIXME: Add this back and change error handling.
    // #[error(transparent)]
    // Other(#[from] anyhow::Error),
}
