use std::error::Error;

#[derive(thiserror::Error, Debug)]
pub enum ServiceError {
    #[error("Invalid argument: {0}")]
    InvalidArgument(String),

    #[error("Conflict: {0}")]
    Conflict(String),

    #[error("Persisted data is corrupted: {0}")]
    DataCorrupted(String),

    #[error(transparent)]
    Internal(#[from] anyhow::Error),
}

pub type ServiceResponse<T> = Result<T, ServiceError>;

pub trait ToServiceResponse<T, E>
where
    E: Error,
{
    fn map_invalid_argument(self, message: &str) -> ServiceResponse<T>;
}

impl<T, E> ToServiceResponse<T, E> for Result<T, E>
where
    E: Error,
{
    fn map_invalid_argument(self, message: &str) -> ServiceResponse<T> {
        self.map_err(|error| ServiceError::InvalidArgument(format!("{}: {}", message, error)))
    }
}
