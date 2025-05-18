use std::{path::PathBuf, sync::Arc};

use axum::{Router, routing::get};
use silver_brain_backend::{SqlService, SqliteConnector};

use crate::{app_state::AppState, route};

pub struct HttpServerConfig {}

pub async fn start_server(_config: HttpServerConfig) {
    let root_path = PathBuf::from("/home/sheep/temp/test");
    let connector = Arc::new(SqliteConnector::new(root_path));
    let service = SqlService::new(connector);

    let state = Arc::new(AppState::new(service));

    let item_route = Router::new()
        .route("/items/{id}", get(route::get_item))
        .route("/items", get(route::get_items))
        .with_state(state.clone());

    let reference_route = Router::new()
        .route("/references", get(route::get_references))
        .with_state(state.clone());

    let routes = Router::new()
        .nest("/api/v2", item_route)
        .nest("/api/v2", reference_route)
        .with_state(state.clone());

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();

    axum::serve(listener, routes).await.unwrap();
}
