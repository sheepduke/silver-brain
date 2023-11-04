use std::sync::Arc;

use axum::{
    extract::{rejection::JsonRejection, Path, State},
    http::{HeaderMap, StatusCode},
    routing::{delete, get, patch, post},
    Json, Router,
};
use silver_brain_core::{EntryCreateRequest, ServiceClientError, StoreName, StoreService};
use tracing::{debug, info, instrument, log::warn};

use crate::{
    error::HttpResponse,
    state::{ServerState, ServerStateArgs},
};

pub fn new(state: Arc<ServerState>) -> Router {
    Router::new()
        .route("/", get(root))
        .route("/api/v2/stores", post(create_store))
        .route("/api/v2/stores", get(list_stores))
        .route("/api/v2/stores", delete(delete_store))
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
        .with_state(state)
}

const STORE_NAME_HEADER: &'static str = "X-SB-StoreName";

fn get_store_name(headers: &HeaderMap) -> Result<StoreName, ServiceClientError> {
    let header_value = headers
        .get(STORE_NAME_HEADER)
        .ok_or(ServiceClientError::InvalidStoreName("".to_string()))?;

    String::from_utf8(header_value.as_bytes().to_vec())
        .map(|value| StoreName(value))
        .map_err(|_| ServiceClientError::InvalidStoreName("Invalid UTF-8 string".to_string()))
}

#[instrument]
async fn root() -> HttpResponse<&'static str> {
    Ok("Silver Brain v2")
}

async fn create_store(
    State(state): State<Arc<ServerState>>,
    headers: HeaderMap,
) -> HttpResponse<StatusCode> {
    let store_name = get_store_name(&headers)?;

    info!(name = store_name.0, "Creating store");

    state.store.create_store(&store_name).await?;

    Ok(StatusCode::CREATED)
}

async fn list_stores() {}

async fn delete_store() {}

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
