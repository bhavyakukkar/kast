use std::path::PathBuf;

#[derive(clap::Subcommand)]
pub enum Command {
    ParseAst,
    Repl { path: Option<PathBuf> },
    Run { path: PathBuf },
}

#[derive(clap::Parser)]
pub struct Args {
    #[clap(subcommand)]
    pub command: Command,
}

pub fn parse() -> Args {
    clap::Parser::parse()
}
