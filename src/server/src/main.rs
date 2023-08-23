use clap::{Args, Parser, Subcommand, ValueEnum};

#[derive(Debug, Parser)]
#[command(name = "silver-brain")]
#[command(long_about = r####"Silver Brain - Your external brain.

This is the CLI program to manipulate Silver Brain."####)]
struct Cli {
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
    },
}

fn main() {
    let cli = Cli::parse();

    println!("{:?}", cli);
}
