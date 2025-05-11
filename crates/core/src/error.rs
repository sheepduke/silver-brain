#[derive(thiserror::Error, Debug)]
pub enum ServiceError {
    #[error("Invalid item id: `{0}`")]
    InvalidId(String),

    #[error("Invalid argument: {0}")]
    InvalidArgument(String),

    #[error("Internal error: {0}")]
    Internal(String),

    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub type ServiceResponse<T> = Result<T, ServiceError>;
