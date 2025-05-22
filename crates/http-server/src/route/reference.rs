// ============================================================
//  Get References
// ============================================================

use std::{str::FromStr, sync::Arc};

use axum::{
    Json,
    extract::{Path, Query, State},
    http::{HeaderMap, StatusCode},
};
use serde::Deserialize;
use silver_brain_core::*;

use crate::app_state::AppState;

use super::util::{ErrorResponse, HttpError, HttpResponse, IdOnly, ToRequestContext};

#[derive(Deserialize)]
pub(crate) struct GetReferencesQuery {
    source: Option<String>,
    target: Option<String>,
}

pub(crate) async fn get_references(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    query: Query<GetReferencesQuery>,
) -> HttpResponse<Json<Vec<ItemReference>>> {
    let context = headers.to_request_context().await?;

    match (&query.source, &query.target) {
        (Some(source), None) => {
            let source = ItemId::from_str(source)?;

            let references = state
                .service
                .get_source_references(&context, &source)
                .await?;

            Ok(Json(references))
        }

        (None, Some(target)) => {
            let target = ItemId::from_str(target)?;

            let references = state
                .service
                .get_target_references(&context, &target)
                .await?;

            Ok(Json(references))
        }

        (Some(_), Some(_)) => Err(HttpError::BadRequest(ErrorResponse::new(
            "Both source and target are provided",
        ))),

        (None, None) => Err(HttpError::BadRequest(ErrorResponse::new(
            "Neither source and target is provided",
        ))),
    }
}

pub(crate) async fn create_reference(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Json(request): Json<CreateItemReferenceRequest>,
) -> HttpResponse<(StatusCode, Json<IdOnly>)> {
    let context = headers.to_request_context().await?;
    let reference_id = state.service.create_reference(&context, request).await?;
    let id_only = IdOnly::from(reference_id.to_string());

    Ok((StatusCode::CREATED, Json(id_only)))
}

pub(crate) async fn update_reference(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(id): Path<String>,
    Json(request): Json<UpdateItemReferenceRequest>,
) -> HttpResponse<StatusCode> {
    let context = headers.to_request_context().await?;
    state
        .service
        .update_reference(&context, &id, request)
        .await?;

    Ok(StatusCode::NO_CONTENT)
}

pub(crate) async fn delete_reference(
    State(state): State<Arc<AppState>>,
    headers: HeaderMap,
    Path(id): Path<String>,
) -> HttpResponse<StatusCode> {
    let context = headers.to_request_context().await?;
    state.service.delete_reference(&context, &id).await?;

    Ok(StatusCode::NO_CONTENT)
}
