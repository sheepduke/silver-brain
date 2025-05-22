mod app_state;
mod route;

mod server;

pub use server::HttpServerConfig;

#[tokio::main]
async fn main() {
    let config = HttpServerConfig {};

    server::start_server(config).await;
}
