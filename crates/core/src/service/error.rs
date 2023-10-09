// ============================================================
//  Error
// ============================================================

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ClientError {
    /// The provided ID is not found.
    #[error("Id is not found")]
    IdNotFound,

    /// The provided file path is not valid.
    #[error("File path is invalid")]
    InvalidFilePath,
}
