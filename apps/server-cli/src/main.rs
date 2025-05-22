use std::path::PathBuf;

use anyhow::{Context, Result, bail};
use clap::Parser;
use silver_brain_server::http::{self, HttpServerConfig};

#[derive(Parser, Debug)]
#[command(version, about, long_about)]
struct Args {
    #[arg(short = 's', long, default_value = "localhost")]
    host: String,

    #[arg(short, long, default_value = "8080")]
    port: u32,

    #[arg(short = 'r', long, help = "Path of data root directory")]
    data_root: Option<String>,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();

    let data_root = match args.data_root {
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

    if !data_root.exists() {
        bail!(
            "Data root directory `{}` does not exist",
            data_root.to_string_lossy()
        );
    }

    let config = HttpServerConfig::builder()
        .host(args.host)
        .port(args.port)
        .data_root(data_root)
        .build();

    http::start_server(config).await;

    Ok(())
}

fn home() -> Result<PathBuf> {
    dirs::home_dir().context("Failed to read HOME directory")
}
