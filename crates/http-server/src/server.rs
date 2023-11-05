use std::{path::PathBuf, sync::Arc};

use axum::{body::Body, http::Request};
use tower_http::{
    trace::{DefaultOnRequest, DefaultOnResponse, TraceLayer},
    LatencyUnit,
};
use tracing::{debug, debug_span, info, Level, Span};

use crate::{
    route,
    state::{ServerState, ServerStateArgs},
};

pub async fn start(data_path: PathBuf, port: u32) {
    let server_state_args = ServerStateArgs::builder().data_path(data_path).build();

    let shared_server_state = Arc::new(ServerState::new(server_state_args));

    let app = route::new(shared_server_state).layer(
        TraceLayer::new_for_http()
            .make_span_with(|request: &Request<Body>| debug_span!("http-request"))
            .on_request(DefaultOnRequest::new().level(Level::INFO))
            .on_request(|request: &Request<Body>, _span: &Span| {
                debug!("started {} {}", request.method(), request.uri().path())
            })
            .on_response(
                DefaultOnResponse::new()
                    .level(Level::INFO)
                    .latency_unit(LatencyUnit::Micros),
            ),
    );

    let address = format!("127.0.0.1:{port}");

    axum::Server::bind(&address.parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}
