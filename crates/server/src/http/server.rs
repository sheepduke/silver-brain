use std::{path::PathBuf, sync::Arc};

use axum::{
    Router,
    routing::{delete, get, patch, post, put},
};
use silver_brain_backend::{SqlService, SqliteConnector};
use typed_builder::TypedBuilder;

use super::{app_state::AppState, route};

#[derive(TypedBuilder)]
pub struct HttpServerConfig {
    #[builder(default = "localhost".to_string(), setter(into))]
    host: String,

    #[builder(default = 8080)]
    port: u32,

    data_root: PathBuf,
}

pub async fn start_server(config: HttpServerConfig) {
    let connector = Arc::new(SqliteConnector::new(config.data_root));
    let service = SqlService::new(connector);

    let state = Arc::new(AppState::new(service));

    let item_route = Router::new()
        .route("/", get(route::get_items))
        .route("/", post(route::create_item))
        .route("/{id}", get(route::get_item))
        .route("/{id}", patch(route::update_item))
        .route("/{id}", delete(route::delete_item))
        .route("/{id}/properties", put(route::upsert_item_property))
        .route(
            "/{id}/properties/{key}",
            delete(route::delete_item_property),
        )
        .route("/{id}/parents/{parent}", put(route::create_parent))
        .route("/{id}/parents/{parent}", delete(route::delete_parent))
        .route("/{id}/children/{child}", put(route::create_child))
        .route("/{id}/children/{child}", delete(route::delete_child))
        .with_state(state.clone());

    let reference_route = Router::new()
        .route("/", get(route::get_references))
        .route("/", post(route::create_reference))
        .route("/{id}", patch(route::update_reference))
        .route("/{id}", delete(route::delete_reference))
        .with_state(state.clone());

    let routes = Router::new()
        .nest("/api/v2/items", item_route)
        .nest("/api/v2/references", reference_route)
        .with_state(state.clone());

    let listener = tokio::net::TcpListener::bind(format!("{}:{}", config.host, config.port))
        .await
        .unwrap();

    axum::serve(listener, routes).await.unwrap();
}
