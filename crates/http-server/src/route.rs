use std::sync::Arc;

use axum::{
    extract::{rejection::JsonRejection, Path},
    http::StatusCode,
    routing::{delete, get, patch, post},
    Json, Router,
};
use silver_brain_core::EntryCreateRequest;

use crate::{
    error::HttpResponse,
    state::{ServerState, ServerStateArgs},
};

pub fn new(args: ServerStateArgs) -> Router {
    Router::new()
        .route("/", get(root))
        .route("/api/v2/entries", post(create_entry))
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
        .with_state(Arc::new(ServerState::new(args)))
}

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
