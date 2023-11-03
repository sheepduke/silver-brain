use crate::route;

pub async fn start(port: u32) {
    let address = format!("127.0.0.1:{port}");
    let app = route::new();

    axum::Server::bind(&address.parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}
