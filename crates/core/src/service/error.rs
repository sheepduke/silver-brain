// ============================================================
//  Error
// ============================================================

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ClientError {
    /// The provided ID is not found.
    #[error("Id is not found")]
    IdNotFound,
}
