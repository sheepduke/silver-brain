use clap::{Args, Parser, Subcommand};
use silver_brain_server as server;

#[derive(Debug, Parser)]
#[command(name = "silver-brain")]
#[command(long_about = r####"Silver Brain - Your external brain.

This is the CLI program to manipulate Silver Brain."####)]
pub struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Server(ServerArgs),
}

/// Server related commands
#[derive(Debug, Args)]
struct ServerArgs {
    #[command(subcommand)]
    command: ServerCommand,
}

#[derive(Debug, Subcommand)]
enum ServerCommand {
    /// Start the server
    Start {
        /// The path of root data directory. Defaults to ~/.silver-brain
        #[arg(short, long)]
        data_path: Option<std::path::PathBuf>,

        /// The port to listen on.
        #[arg(short, long)]
        #[clap(default_value_t = 5000)]
        port: u32,
    },
}

pub async fn handle_cli(cli: &Cli) {
    match &cli.command {
        Command::Server(args) => match &args.command {
            ServerCommand::Start { data_path: _, port } => server::start(*port).await,
        },
    }
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    handle_cli(&cli).await;
}
