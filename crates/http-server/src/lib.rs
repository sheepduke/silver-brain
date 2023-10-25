use axum::{
    extract::{rejection::JsonRejection, Path},
    http::StatusCode,
    response::IntoResponse,
    routing::{delete, get, patch, post},
    Json, Router,
};
use silver_brain_core::{EntryCreateRequest, ServiceClientError};

pub fn new_app() -> Router {
    Router::new()
        .route("/", get(root))
        // .route("/api/v2/entries", post(create_entry))
        .route("/api/v2/entries/:id", get(get_entry))
        .route("/api/v2/entries/:id", patch(update_entry))
        .route("/api/v2/entries/:id", delete(delete_entry))
        .route("/api/v2/links", post(create_link))
        .route("/api/v2/links/:id", get(get_link))
        .route("/api/v2/links/:id", patch(update_link))
        .route("/api/v2/links/:id", delete(delete_entry))
        .route("/api/v2/attachments", post(create_attachment))
        .route("/api/v2/attachments/:id", get(get_attachment))
        .route("/api/v2/attachments/:id", patch(update_attachment))
        .route("/api/v2/attachments/:id", delete(delete_attachment))
}

pub async fn start(port: u32) {
    let address = format!("127.0.0.1:{port}");
    let app = new_app();

    axum::Server::bind(&address.parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}

enum ErrorKind {
    NotFound,
    BadRequest,
    InternalError,
}

impl From<ServiceClientError> for ErrorKind {
    fn from(value: ServiceClientError) -> Self {
        match value {
            ServiceClientError::NotFound(_) => Self::NotFound,
            ServiceClientError::BadArguments(_) => Self::BadRequest,
            ServiceClientError::InvalidStoreName(_) => Self::BadRequest,
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

type HttpResponse<T> = Result<T, ErrorKind>;

async fn root() -> HttpResponse<&'static str> {
    Ok("Silver Brain v2")
}

async fn create_entry(
    payload: Result<Json<serde_json::Value>, JsonRejection>,
) -> HttpResponse<StatusCode> {
    let Json(json) = payload?;

    let request = serde_json::from_value::<EntryCreateRequest>(json)?;

    Ok(StatusCode::CREATED)
}

async fn get_entry(Path(id): Path<String>) -> String {
    todo!()
}

async fn update_entry() {}

async fn delete_entry() {}

async fn create_link() {}

async fn get_link() {}

async fn update_link() {}

async fn delete_link() {}

async fn create_attachment() {}

async fn get_attachment() {}

async fn update_attachment() {}

async fn delete_attachment() {}
