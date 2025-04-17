use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ServiceError {
    #[error("Invalid item id: `{0}`")]
    InvalidId(String),

    #[error("Invalid argument: {0}")]
    InvalidArgument(String),
}

pub type ServiceResponse<T> = Result<T, ServiceError>;
