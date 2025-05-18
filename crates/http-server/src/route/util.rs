use std::str::FromStr;

use axum::{
    Json,
    http::{HeaderMap, StatusCode},
    response::{IntoResponse, Response},
};

use serde::Serialize;

use silver_brain_core::*;

// ============================================================
//  ToRequestContext
// ============================================================

pub(crate) trait ToRequestContext {
    async fn to_request_context(&self) -> HttpResponse<RequestContext>;
}

impl ToRequestContext for HeaderMap {
    async fn to_request_context(&self) -> HttpResponse<RequestContext> {
        let repo_name_str = match self.get("X-SB-Repo") {
            Some(repo_name) => repo_name
                .to_str()
                .map_invalid_argument("Invalid X-SB-RepoName header value"),

            // })
            None => Ok("main"),
        }?;

        let repo_name = RepoName::from_str(repo_name_str)?;

        Ok(RequestContext::builder().repo_name(repo_name).build())
    }
}

// ============================================================
//  HttpResponse
// ============================================================

pub(crate) type HttpResponse<T> = Result<T, HttpError>;

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub(crate) struct ErrorResponse {
    message: String,
}

impl ErrorResponse {
    pub(crate) fn new(message: String) -> Self {
        Self { message }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum HttpError {
    NotFound,
    BadRequest(ErrorResponse),
    Conflict(ErrorResponse),
    Internal(ErrorResponse),
}

impl From<ServiceError> for HttpError {
    fn from(value: ServiceError) -> Self {
        match value {
            ServiceError::InvalidArgument(message) => Self::BadRequest(ErrorResponse::new(message)),
            ServiceError::Internal(error) => Self::Internal(ErrorResponse::new(error.to_string())),
        }
    }
}

impl IntoResponse for HttpError {
    fn into_response(self) -> Response {
        match self {
            Self::NotFound => StatusCode::NOT_FOUND.into_response(),
            Self::BadRequest(error) => (StatusCode::BAD_REQUEST, Json(error)).into_response(),
            Self::Internal(error) => {
                (StatusCode::INTERNAL_SERVER_ERROR, Json(error)).into_response()
            }
            _ => todo!(),
        }
    }
}

// ============================================================
//  Functions
// ============================================================

pub(crate) fn split_comma(s: &str) -> Vec<String> {
    s.split(",")
        .map(|it| it.trim())
        .filter(|it| !it.is_empty())
        .map(|it| it.to_owned())
        .collect()
}
