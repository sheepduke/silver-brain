use std::{path::PathBuf, sync::Arc};

use silver_brain_sql_service::SqliteStoreOptions;

use crate::{
    route,
    state::{ServerState, ServerStateArgs},
};

pub async fn start(port: u32) {
    let app = route::new(
        ServerStateArgs::builder()
            .data_path("~/.silver-brain-dev".into())
            .store_options(SqliteStoreOptions::builder().auto_create(true).auto_migrate(true).build()
            ).build());

    let address = format!("127.0.0.1:{port}");

    axum::Server::bind(&address.parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}
