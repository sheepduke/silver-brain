use std::{path::PathBuf, sync::Arc};

use axum::{
    Router,
    routing::{delete, get, patch, post, put},
};
use silver_brain_backend::{SqlService, SqliteConnector};

use crate::{app_state::AppState, route};

pub struct HttpServerConfig {}

pub async fn start_server(_config: HttpServerConfig) {
    let root_path = PathBuf::from("/home/sheep/temp/test");
    let connector = Arc::new(SqliteConnector::new(root_path));
    let service = SqlService::new(connector);

    let state = Arc::new(AppState::new(service));

    let item_route = Router::new()
        .route("/items", get(route::get_items))
        .route("/items", post(route::create_item))
        .route("/items/{id}", get(route::get_item))
        .route("/items/{id}", patch(route::update_item))
        .route("/items/{id}", delete(route::delete_item))
        .route("/items/{id}/properties", put(route::upsert_item_property))
        .route(
            "/items/{id}/properties/{key}",
            delete(route::delete_item_property),
        )
        .route("/items/{id}/parents/{parent}", put(route::create_parent))
        .route("/items/{id}/parents/{parent}", delete(route::delete_parent))
        .route("/items/{id}/children/{child}", put(route::create_child))
        .route("/items/{id}/children/{child}", delete(route::delete_child))
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
