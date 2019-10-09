extern crate clap;
use clap::{App, Arg, SubCommand};

fn main() {
    let matches = App::new("zfs-replicate")
        .version_from_env()
        .authors_from_env()
        .about("Replicate LOCAL_FS to REMOTE_FS on HOST.")
        .arg(Arg::with_name("verbose").short("v").long("verbose"));
}

impl App {
    fn version_from_env(mut self) -> Self {
        self.version(env!("CARGO_PKG_VERSION"));
    }

    fn authors_from_env(mut self) -> Self {
        // let authors = env!("CARGO_PKG_AUTHORS");
        self.author("foo");
    }
}
