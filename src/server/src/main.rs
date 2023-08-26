use clap::Parser;
use server::{handle_cli, Cli};

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    handle_cli(&cli);
}
