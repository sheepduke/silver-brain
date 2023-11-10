use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use axum::{
    extract::{rejection::JsonRejection, Path, Query, State},
    http::{HeaderMap, StatusCode},
    routing::{delete, get, patch, post},
    Json, Router,
};
use silver_brain_core::service::{
    EntryCreateRequest, EntryLoadOptions, EntryService, RequestContext, ServiceError, StoreName,
    StoreService,
};
use silver_brain_core::{
    domain::{Entry, EntryId},
    service::ServiceResult,
};

use tracing::{debug, info, instrument, log::warn};

use crate::{
    error::{ErrorKind, HttpResponse},
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

fn get_store_name(headers: &HeaderMap) -> Result<StoreName, ServiceError> {
    let header_value = headers
        .get(STORE_NAME_HEADER)
        .ok_or(ServiceError::InvalidStoreName)?;

    String::from_utf8(header_value.as_bytes().to_vec())
        .map(|value| StoreName(value))
        .map_err(|_| ServiceError::InvalidStoreName)
}

fn create_request_context(headers: &HeaderMap) -> ServiceResult<RequestContext> {
    let store_name = get_store_name(&headers)?;

    Ok(RequestContext::builder().store_name(store_name).build())
}

fn create_entry_load_options(params: &HashMap<String, String>) -> EntryLoadOptions {
    let load_params = params
        .get("load")
        .unwrap_or(&"".to_string())
        .split(",")
        .map(|x| x.to_string())
        .collect::<HashSet<String>>();

    EntryLoadOptions::builder()
        .load_content(load_params.contains("contents"))
        .load_attachments(load_params.contains("attachments"))
        .load_times(load_params.contains("times"))
        .build()
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

async fn list_stores(State(state): State<Arc<ServerState>>) -> HttpResponse<Json<Vec<String>>> {
    let stores = state
        .store
        .list_stores()
        .await?
        .into_iter()
        .map(|x| x.0)
        .collect::<Vec<String>>();

    Ok(Json(stores))
}

async fn delete_store(
    State(state): State<Arc<ServerState>>,
    headers: HeaderMap,
) -> HttpResponse<StatusCode> {
    let store_name = get_store_name(&headers)?;

    state.store.delete_store(&store_name).await?;

    Ok(StatusCode::NO_CONTENT)
}

#[instrument]
async fn create_entry(
    State(state): State<Arc<ServerState>>,
    headers: HeaderMap,
    payload: Result<Json<serde_json::Value>, JsonRejection>,
) -> HttpResponse<(StatusCode, String)> {
    let Json(json) = payload?;

    let request = serde_json::from_value::<EntryCreateRequest>(json)?;
    let context = create_request_context(&headers)?;

    let EntryId(entry_id) = state.entry_service.create_entry(&context, request).await?;

    Ok((StatusCode::CREATED, entry_id))
}

#[instrument]
async fn get_entry(
    State(state): State<Arc<ServerState>>,
    headers: HeaderMap,
    Path(id): Path<String>,
    Query(params): Query<HashMap<String, String>>,
) -> HttpResponse<Json<Entry>> {
    let context = create_request_context(&headers)?;

    let options = create_entry_load_options(&params);

    let entry = state
        .entry_service
        .get_entry(&context, &EntryId(id), &options)
        .await?;

    Ok(Json(entry))
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
