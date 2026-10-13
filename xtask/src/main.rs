//! Repository tasks. No compiler chapter depends on this crate.
mod chapters;
mod pdf;
mod syntax;

use std::{env, path::PathBuf, process::ExitCode};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn run() -> Result<()> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    env::set_current_dir(&root)?;
    let args: Vec<_> = env::args().skip(1).collect();
    match args.iter().map(String::as_str).collect::<Vec<_>>().as_slice() {
        ["syntax"] => syntax::generate(&root,false),
        ["syntax","--check"] => syntax::generate(&root,true),
        ["pdf-collect"] => pdf::collect(&root),
        ["new-chapter", number, name] => chapters::new_chapter(&root, number.parse()?, name),
        ["check-inherit", number] => chapters::check_inherit(&root, number.parse()?, false),
        ["check-inherit", number, "--full"] => chapters::check_inherit(&root, number.parse()?, true),
        ["book-diff", number] => chapters::book_diff(&root, number.parse()?),
        _ => Err("usage: cargo xtask {new-chapter NN name | check-inherit NN [--full] | book-diff NN | syntax [--check] | pdf-collect}".into()),
    }
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("xtask: {e}");
            ExitCode::FAILURE
        }
    }
}
