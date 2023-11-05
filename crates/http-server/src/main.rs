use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};
use tracing::{info, info_span, instrument, Level};

#[derive(Debug, Parser)]
#[command(name = "silver-brain-server")]
#[command(long_about = r####"Silver Brain Server

Starts a server that serves all sort of clients!
"####)]
pub struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Starts the server.
    Start {
        /// The path of root data directory. Defaults to ~/.silver-brain
        #[arg(short, long)]
        data_path: Option<String>,

        /// The port to listen on.
        #[arg(short, long)]
        #[clap(default_value_t = 5000)]
        port: u32,
    },
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    tracing_subscriber::fmt()
        .with_max_level(Level::DEBUG)
        .init();

    match &cli.command {
        Command::Start { data_path, port } => {
            let path = match data_path {
                Some(value) => value,
                None => "~/.silver-brain",
            };

            let full_path = shellexpand::tilde(path);

            silver_brain_http_server::server::start((*full_path).into(), *port).await
        }
    }
}
