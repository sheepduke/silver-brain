use serde::Deserialize;
use silver_brain_core::*;
use std::sync::Arc;

use axum::{
    Json, debug_handler,
    extract::{Path, Query, State},
    http::HeaderMap,
};

use crate::{app_state::AppState, route::util::HttpError};

use super::util::{HttpResponse, ToRequestContext};

pub(crate) async fn hello() -> &'static str {
    "Hello, world"
}

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

    let load_options = match &query.select {
        Some(select) => parse_item_load_options(select)?,
        None => ItemLoadOptions::core(),
    };

    let item_opt = state.service.get_item(&context, &id, &load_options).await?;

    match item_opt {
        Some(item) => Ok(Json(item)),
        None => Err(HttpError::NotFound),
    }
}

fn parse_item_load_options(input: &str) -> ServiceResponse<ItemLoadOptions> {
    let props = input
        .split(",")
        .map(|it| it.trim())
        .filter(|it| !it.is_empty());

    let mut options = ItemLoadOptions::core();

    for prop in props {
        match prop {
            "content_type" | "content-type" | "contentType" => options.load_content_type = true,
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
