use anyhow::Error;
use axum::{extract::rejection::JsonRejection, http::StatusCode, response::IntoResponse};
use silver_brain_core::ServiceClientError;

pub enum ErrorKind {
    NotFound,
    BadRequest,
    InternalError,
}

impl From<Error> for ErrorKind {
    fn from(value: Error) -> Self {
        tracing::debug!("I do not believe this...");
        tracing::debug!("{:?}", value);
        Self::InternalError
    }
}

impl From<ServiceClientError> for ErrorKind {
    fn from(value: ServiceClientError) -> Self {
        tracing::debug!("Enter conversion!!!");

        match value {
            ServiceClientError::NotFound(_) => Self::NotFound,
            ServiceClientError::BadArguments(_) => Self::BadRequest,
            ServiceClientError::InvalidStoreName(_) => {
                tracing::debug!("Really??");
                Self::BadRequest
            }
            ServiceClientError::InvalidAttachmentFilePath(_) => Self::BadRequest,
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
            Self::InternalError => StatusCode::INTERNAL_SERVER_ERROR,
        }
        .into_response()
    }
}

pub type HttpResponse<T> = Result<T, ErrorKind>;
