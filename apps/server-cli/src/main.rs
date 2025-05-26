use std::path::PathBuf;

use anyhow::{Context, Result, bail};
use clap::Parser;
use silver_brain_server::http::{self, HttpServerConfig};
use tracing::info;
use tracing_subscriber::{EnvFilter, layer::SubscriberExt, util::SubscriberInitExt};

#[derive(Parser, Debug)]
#[command(version, about, long_about)]
struct Args {
    #[arg(short = 's', long, default_value = "localhost")]
    host: String,

    #[arg(short, long, default_value = "8080")]
    port: u32,

    #[arg(short = 'r', long, help = "Path of data root directory")]
    data_root: Option<String>,

    #[arg(short = 'd', long, help = "Set log level to debug")]
    debug: bool,

    #[arg(short = 'v', long, help = "Set log level to verbose")]
    verbose: bool,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();

    // Handle data root.
    let data_root = get_data_root(&args)?;

    if !data_root.exists() {
        bail!(
            "Data root directory `{}` does not exist",
            data_root.to_string_lossy()
        );
    }

    // Handle log level.
    let log_level = get_log_level(&args);
    let log_filter = EnvFilter::new(log_level);

    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer())
        .with(log_filter)
        .init();

    info!("Listening on http://{}:{}", args.host, args.port);

    let config = HttpServerConfig::builder()
        .host(args.host)
        .port(args.port)
        .data_root(data_root)
        .build();

    http::start_server(config).await;

    Ok(())
}

fn get_data_root(args: &Args) -> Result<PathBuf> {
    // Handle data root.
    let data_root = match &args.data_root {
        Some(data_root) => {
            if data_root.starts_with("~/") {
                let mut path = home()?;
                let (_, part) = data_root.split_at(1);
                path.push(part);
                path
            } else {
                PathBuf::from(data_root)
            }
        }
        None => {
            let mut path = home()?;
            path.push(".silver-brain");
            path
        }
    };

    Ok(data_root)
}

fn get_log_level(args: &Args) -> &str {
    if args.verbose {
        "trace"
    } else if args.debug {
        "debug"
    } else {
        "info"
    }
}

fn home() -> Result<PathBuf> {
    dirs::home_dir().context("Failed to read HOME directory")
}
