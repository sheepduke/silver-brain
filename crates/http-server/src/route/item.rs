use serde::Deserialize;
use silver_brain_core::*;
use std::sync::Arc;

use axum::{
    Json, debug_handler,
    extract::{Path, Query, State},
    http::HeaderMap,
};

use crate::{app_state::AppState, route::util::HttpError};

use super::util::{self, ErrorResponse, HttpResponse, ToRequestContext};

// ============================================================
//  Get Item
// ============================================================

#[derive(Deserialize)]
pub(crate) struct GetItemQuery {
    pub select: Option<String>,
}

#[debug_handler]
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
//  Get Items
// ============================================================

#[derive(Deserialize)]
pub(crate) struct GetItemsQuery {
    pub ids: Option<String>,

    pub search: Option<String>,

    pub select: Option<String>,
}

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
                .search_items(&context, &search, &load_options)
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
