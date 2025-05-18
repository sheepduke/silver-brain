// ============================================================
//  Get References
// ============================================================

use std::sync::Arc;

use axum::{Json, extract::State};
use silver_brain_core::ItemReference;

use crate::app_state::AppState;

use super::util::HttpResponse;

pub(crate) async fn get_references(
    State(state): State<Arc<AppState>>,
) -> HttpResponse<Json<Vec<ItemReference>>> {
    let references: Vec<ItemReference> = Vec::new();

    Ok(Json(references))
}
