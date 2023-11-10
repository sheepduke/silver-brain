use anyhow::Error;
use axum::{extract::rejection::JsonRejection, http::StatusCode, response::IntoResponse};
use silver_brain_core::service::ServiceError;

pub enum ErrorKind {
    NotFound,
    BadRequest,
    InternalError(anyhow::Error),
}

impl From<Error> for ErrorKind {
    fn from(value: Error) -> Self {
        tracing::debug!("I do not believe this...");
        tracing::debug!("{:?}", value);
        Self::InternalError(value)
    }
}

impl From<ServiceError> for ErrorKind {
    fn from(value: ServiceError) -> Self {
        tracing::debug!("Enter conversion!!!");

        match value {
            ServiceError::NotFound => Self::NotFound,
            ServiceError::BadArguments => Self::BadRequest,
            ServiceError::InvalidStoreName => Self::BadRequest,
            ServiceError::InvalidAttachmentFilePath => Self::BadRequest,
            ServiceError::Other(error) => Self::InternalError(error),
        }
    }
}

impl From<JsonRejection> for ErrorKind {
    fn from(value: JsonRejection) -> Self {
        Self::BadRequest
    }
}

impl From<serde_json::Error> for ErrorKind {
    fn from(value: serde_json::Error) -> Self {
        Self::BadRequest
    }
}

impl IntoResponse for ErrorKind {
    fn into_response(self) -> axum::response::Response {
        match self {
            Self::NotFound => StatusCode::NOT_FOUND,
            Self::BadRequest => StatusCode::BAD_REQUEST,
            Self::InternalError(_) => StatusCode::INTERNAL_SERVER_ERROR,
        }
        .into_response()
    }
}

pub type HttpResponse<T> = Result<T, ErrorKind>;
