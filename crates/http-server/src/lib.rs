use axum::{
    extract::Path,
    routing::{delete, get, patch, post},
    Router,
};

pub fn new_app() -> Router {
    Router::new()
        .route("/", get(root))
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
}

pub async fn start(port: u32) {
    let address = format!("127.0.0.1:{port}");
    let app = new_app();

    axum::Server::bind(&address.parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}

async fn root() -> &'static str {
    "Silver Brain v2"
}

async fn create_entry() {}

async fn get_entry(Path(id): Path<String>) -> String {
    todo!()
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
