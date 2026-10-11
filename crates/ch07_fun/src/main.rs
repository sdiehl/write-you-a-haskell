//! A small driver stops at pass boundaries; only this module reads files or renders errors.
use ch07_fun::{
    elaborate, eval, lexer, parse,
    session::{Options, Session},
    source::FileId,
    typecheck,
};
use clap::{Parser, Subcommand, ValueEnum};
use rustyline::{DefaultEditor, error::ReadlineError};
use std::{
    fs,
    io::{self, IsTerminal},
    path::PathBuf,
    process::ExitCode,
};
#[derive(Clone, Copy, Debug, ValueEnum)]
enum Pass {
    Tokens,
    Parse,
    Types,
    Core,
}
#[derive(Clone, Copy, ValueEnum)]
enum Color {
    Auto,
    Always,
    Never,
}
#[derive(Clone, Copy, ValueEnum)]
enum Engine {
    Core,
}
#[derive(Parser)]
#[command(name = "fun-ch07", about = "Chapter 7: the first Fun compiler")]
struct Cli {
    #[command(subcommand)]
    command: Command,
    #[arg(long, global = true)]
    lint: bool,
    #[arg(long, global = true)]
    no_prelude: bool,
    #[arg(long, global = true, value_enum, default_value = "auto")]
    color: Color,
    #[arg(long, global = true, value_enum)]
    dump: Option<Pass>,
    #[arg(long, global = true, default_value_t = 1000000)]
    fuel: usize,
}
#[derive(Subcommand)]
enum Command {
    Check {
        file: PathBuf,
    },
    Run {
        file: PathBuf,
        #[arg(long, value_enum, default_value = "core")]
        engine: Engine,
    },
    Dump {
        #[arg(long, value_enum)]
        pass: Pass,
        file: PathBuf,
    },
    Repl,
}
fn compile(
    sess: &mut Session,
    file: FileId,
    pass: Option<Pass>,
    run: bool,
    fuel: usize,
) -> Result<(), ch07_fun::session::Stop> {
    if matches!(pass, Some(Pass::Tokens)) {
        for (l, t, r) in lexer::lex(sess, file)? {
            println!("{l}..{r}: {t:?}");
        }
        return Ok(());
    }
    let program = parse::parse(sess, file)?;
    if matches!(pass, Some(Pass::Parse)) {
        println!("{{");
        for b in program.bindings {
            println!("  {} = {};", b.name, b.rhs);
        }
        println!("}}");
        return Ok(());
    }
    let typed = typecheck::check(sess, program)?;
    if matches!(pass, Some(Pass::Types)) {
        println!("main : {}", typed.main_type);
        return Ok(());
    }
    if !run && pass.is_none() {
        return Ok(());
    }
    let core = elaborate::elaborate(sess, typed)?;
    if sess.options.lint {
        ch07_fun::core::lint(&core).expect("internal error: Core lint");
    }
    if matches!(pass, Some(Pass::Core)) {
        for b in core.bindings {
            println!(
                "{} {} = {}",
                if b.recursive { "rec" } else { "val" },
                b.name,
                b.rhs
            );
        }
        return Ok(());
    }
    let result = std::thread::Builder::new()
        .stack_size(512 * 1024 * 1024)
        .spawn(move || eval::run(&core, fuel))
        .expect("Core interpreter thread")
        .join()
        .expect("Core interpreter panic");
    match result {
        Ok(value) => {
            println!("{value}");
            Ok(())
        }
        Err(message) => {
            eprintln!("fun: {message}");
            Err(ch07_fun::session::Stop)
        }
    }
}
fn process(
    source: String,
    name: String,
    cli: &Cli,
    run: bool,
    pass: Option<Pass>,
) -> bool {
    let mut sess = Session {
        options: Options {
            lint: cli.lint,
            no_prelude: cli.no_prelude,
            dump: pass.map(|p| format!("{p:?}")),
        },
        ..Session::default()
    };
    let file = sess.sources.add(name, source);
    let success = compile(&mut sess, file, pass, run, cli.fuel).is_ok();
    let color = matches!(cli.color, Color::Always)
        || matches!(cli.color, Color::Auto) && io::stderr().is_terminal();
    for diagnostic in &sess.diagnostics {
        let _ =
            diagnostic.render(&sess.sources, color, io::stderr().lock());
    }
    success
}
fn repl(cli: &Cli) -> Result<(), Box<dyn std::error::Error>> {
    let mut editor = DefaultEditor::new()?;
    println!("Fun: expressions, :type EXPR, :core EXPR, :quit.");
    loop {
        match editor.readline("fun> ") {
            Ok(line) if line.trim() == ":quit" => break,
            Ok(line) => {
                if line.trim().is_empty() {
                    continue;
                }
                editor.add_history_entry(&line)?;
                let (source, pass) =
                    if let Some(s) = line.strip_prefix(":type ") {
                        (s, Some(Pass::Types))
                    } else if let Some(s) = line.strip_prefix(":core ") {
                        (s, Some(Pass::Core))
                    } else {
                        (line.as_str(), cli.dump)
                    };
                process(
                    format!("{{ main = {source}; }}"),
                    "<repl>".into(),
                    cli,
                    true,
                    pass,
                );
            }
            Err(ReadlineError::Eof | ReadlineError::Interrupted) => break,
            Err(e) => return Err(e.into()),
        }
    }
    Ok(())
}
fn main() -> ExitCode {
    let cli = Cli::parse();
    let (file, run, pass) = match &cli.command {
        Command::Check { file } => (file, false, cli.dump),
        Command::Run { file, .. } => (file, true, cli.dump),
        Command::Dump { file, pass } => (file, false, Some(*pass)),
        Command::Repl => {
            return match repl(&cli) {
                Ok(()) => ExitCode::SUCCESS,
                Err(e) => {
                    eprintln!("fun: {e}");
                    ExitCode::FAILURE
                }
            };
        }
    };
    match fs::read_to_string(file) {
        Ok(source) => {
            if process(source, file.display().to_string(), &cli, run, pass)
            {
                ExitCode::SUCCESS
            } else {
                ExitCode::FAILURE
            }
        }
        Err(e) => {
            eprintln!("fun: {e}");
            ExitCode::FAILURE
        }
    }
}
