// ============================================================
//  Get References
// ============================================================

use std::{str::FromStr, sync::Arc};

use axum::{
    Json,
    extract::{Query, State},
    http::HeaderMap,
};
use serde::Deserialize;
use silver_brain_core::*;

use crate::app_state::AppState;

use super::util::{ErrorResponse, HttpError, HttpResponse, ToRequestContext};

// ============================================================
//  Get References
// ============================================================

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
            let source = ItemId::from_str(&source)?;

            let references = state
                .service
                .get_source_references(&context, &source)
                .await?;

            Ok(Json(references))
        }

        (None, Some(target)) => {
            let target = ItemId::from_str(&target)?;

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
