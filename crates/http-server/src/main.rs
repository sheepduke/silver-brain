use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

#[derive(Debug, Parser)]
#[command(name = "silver-brain-server")]
#[command(long_about = r####"Silver Brain Server

Starts a server that serves all sort of clients!
"####)]
pub struct Cli 
{
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Starts the server.
    Start {
        /// The path of root data directory. Defaults to ~/.silver-brain
        #[arg(short, long)]
        data_path: Option<PathBuf>,

        /// The port to listen on.
        #[arg(short, long)]
        #[clap(default_value_t = 5000)]
        port: u32,
    },
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Command::Start { data_path: _, port } => silver_brain_http_server::server::start(*port).await,
    }
}
