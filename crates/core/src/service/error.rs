// ============================================================
//  Error
// ============================================================

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ServiceError {
    /// The provided ID is not found.
    #[error("Id is not found")]
    IdNotFound,
}
