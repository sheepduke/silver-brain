use std::{path::PathBuf, sync::Arc};

use tower_http::trace::TraceLayer;

use crate::{
    route,
    state::{ServerState, ServerStateArgs},
};

pub async fn start(data_path: PathBuf, port: u32) {
    let server_state_args = ServerStateArgs::builder().data_path(data_path).build();

    let shared_server_state = Arc::new(ServerState::new(server_state_args));

    let app = route::new(shared_server_state).layer(TraceLayer::new_for_http());

    let address = format!("127.0.0.1:{port}");

    axum::Server::bind(&address.parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}
