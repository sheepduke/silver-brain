use serde::Deserialize;
use silver_brain_core::*;
use std::sync::Arc;
use tracing::instrument;

use axum::{
    Json,
    extract::{Path, Query, State},
    http::{HeaderMap, StatusCode},
};

use super::{
    super::app_state::AppState,
    super::route::util::{HttpError, IdOnly},
};

use super::util::{self, ErrorResponse, HttpResponse, ToRequestContext};

// ============================================================
//  Item
// ============================================================

#[derive(Debug, Deserialize)]
pub(crate) struct GetItemQuery {
    pub select: Option<String>,
}

#[instrument]
pub(crate) async fn get_item(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(id): Path<String>,
    query: Query<GetItemQuery>,
) -> HttpResponse<Json<Item>> {
    let context = headers.to_request_context().await?;
    let load_options = parse_item_load_options(&query.select.as_deref())?;
    let item_opt = state.service.get_item(&context, &id, &load_options).await?;

    match item_opt {
        Some(item) => Ok(Json(item)),
        None => Err(HttpError::NotFound),
    }
}

#[derive(Debug, Deserialize)]
pub(crate) struct GetItemsQuery {
    pub ids: Option<String>,

    pub search: Option<String>,

    pub select: Option<String>,
}

#[instrument]
pub(crate) async fn get_items(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    query: Query<GetItemsQuery>,
) -> HttpResponse<Json<Vec<Item>>> {
    let context = headers.to_request_context().await?;
    let load_options = parse_item_load_options(&query.select.as_deref())?;

    match (&query.ids, &query.search) {
        // Search only.
        (None, Some(search)) => {
            let items = state
                .service
                .search_items(&context, search, &load_options)
                .await?;

            Ok(Json(items))
        }

        // Ids only.
        (Some(ids), None) => {
            let ids = util::split_comma(ids);
            let ids: Vec<&str> = ids.iter().map(String::as_str).collect();

            let items = state
                .service
                .get_items(&context, &ids, &load_options)
                .await?;

            Ok(Json(items))
        }

        // Invalid.
        (Some(_), Some(_)) => Err(HttpError::BadRequest(ErrorResponse::new(
            "Both `ids` and `search` are specified",
        ))),

        (None, None) => Err(HttpError::BadRequest(ErrorResponse::new(
            "Neither `ids` or `search` is specified",
        ))),
    }
}

#[instrument]
pub(crate) async fn create_item(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Json(request): Json<CreateItemRequest>,
) -> HttpResponse<(StatusCode, Json<IdOnly>)> {
    let context = headers.to_request_context().await?;
    let item_id = state.service.create_item(&context, request).await?;
    let id_only = IdOnly::from(item_id.to_string());

    Ok((StatusCode::CREATED, Json(id_only)))
}

#[instrument]
pub(crate) async fn update_item(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(id): Path<String>,
    Json(request): Json<UpdateItemRequest>,
) -> HttpResponse<StatusCode> {
    let context = headers.to_request_context().await?;
    state.service.update_item(&context, &id, request).await?;

    Ok(StatusCode::NO_CONTENT)
}

#[instrument]
pub(crate) async fn delete_item(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(id): Path<String>,
) -> HttpResponse<StatusCode> {
    let context = headers.to_request_context().await?;
    state.service.delete_item(&context, &id).await?;

    Ok(StatusCode::NO_CONTENT)
}

fn parse_item_load_options(input: &Option<&str>) -> ServiceResponse<ItemLoadOptions> {
    match input {
        Some(select) => {
            let props = util::split_comma(select);

            let mut options = ItemLoadOptions::core();

            for prop in props {
                match prop.as_str() {
                    "content_type" | "content-type" | "contentType" => {
                        options.load_content_type = true
                    }
                    "content" => options.load_content = true,
                    "create-time" | "create_time" | "createTime" => options.load_create_time = true,
                    "update-time" | "update_time" | "updateTime" => options.load_update_time = true,
                    "properties" => options.load_properties = true,
                    "parents" => options.load_parents = true,
                    "children" => options.load_children = true,
                    "all" => options = ItemLoadOptions::all(),
                    _ => {
                        return Err(ServiceError::InvalidArgument(format!(
                            "Invalid select property `{}`",
                            prop
                        )));
                    }
                }
            }

            Ok(options)
        }
        None => Ok(ItemLoadOptions::core()),
    }
}

// ============================================================
//  Item Property
// ============================================================

#[instrument]
pub(crate) async fn upsert_item_property(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(id): Path<String>,
    Json(request): Json<UpsertItemPropertyRequest>,
) -> HttpResponse<StatusCode> {
    let context = headers.to_request_context().await?;
    state
        .service
        .upsert_item_property(&context, &id, request)
        .await?;

    Ok(StatusCode::NO_CONTENT)
}

#[instrument]
pub(crate) async fn delete_item_property(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path((id, key)): Path<(String, String)>,
) -> HttpResponse<StatusCode> {
    let context = headers.to_request_context().await?;

    state
        .service
        .delete_item_property(&context, &id, &key)
        .await?;

    Ok(StatusCode::NO_CONTENT)
}

// ============================================================
//  Item Link
// ============================================================

#[instrument]
pub(crate) async fn create_parent(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path((id, parent)): Path<(String, String)>,
) -> HttpResponse<StatusCode> {
    let context = headers.to_request_context().await?;

    state.service.create_link(&context, &parent, &id).await?;

    Ok(StatusCode::NO_CONTENT)
}

#[instrument]
pub(crate) async fn create_child(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path((id, child)): Path<(String, String)>,
) -> HttpResponse<StatusCode> {
    let context = headers.to_request_context().await?;

    state.service.create_link(&context, &id, &child).await?;

    Ok(StatusCode::NO_CONTENT)
}

#[instrument]
pub(crate) async fn delete_parent(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path((id, parent)): Path<(String, String)>,
) -> HttpResponse<StatusCode> {
    let context = headers.to_request_context().await?;

    state.service.delete_link(&context, &parent, &id).await?;

    Ok(StatusCode::NO_CONTENT)
}

#[instrument]
pub(crate) async fn delete_child(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path((id, child)): Path<(String, String)>,
) -> HttpResponse<StatusCode> {
    let context = headers.to_request_context().await?;

    state.service.delete_link(&context, &id, &child).await?;

    Ok(StatusCode::NO_CONTENT)
}
