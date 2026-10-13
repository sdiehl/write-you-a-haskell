//! Inspect terms, run either strategy, or trace normal-order steps in the REPL.
use ch06_poly::{
    cbn, cek, diagnostic::Diagnostic, eval, lexer, names, normal, parse,
    types,
};
use clap::{Parser, Subcommand, ValueEnum};
use rustyline::{DefaultEditor, error::ReadlineError};
use std::{
    fs,
    io::{self, IsTerminal},
    path::PathBuf,
    process::ExitCode,
};

#[derive(Clone, Copy, ValueEnum)]
enum Pass {
    TypesW,
    Tokens,
    Parse,
    Debruijn,
    Types,
}

#[derive(Clone, Copy, Default, ValueEnum)]
enum Engine {
    #[default]
    Core,
    Normal,
    Cek,
    Cbn,
}

#[derive(Clone, Copy, ValueEnum)]
enum Color {
    Auto,
    Always,
    Never,
}

#[derive(Parser)]
#[command(name = "fun-ch06", about = "Chapter 6: Hindley–Milner inference")]
struct Cli {
    #[command(subcommand)]
    command: Command,
    #[arg(long, global = true, value_enum, default_value = "auto")]
    color: Color,
    #[arg(long, global = true)]
    lint: bool,
    #[arg(long, global = true, default_value_t = 100000000)]
    fuel: usize,
    #[arg(long, global = true, value_enum)]
    dump: Option<Pass>,
}

#[derive(Subcommand)]
enum Command {
    Check {
        file: PathBuf,
    },
    Run {
        file: PathBuf,
        #[arg(long, value_enum, default_value = "cek")]
        engine: Engine,
    },
    Dump {
        #[arg(long, value_enum)]
        pass: Pass,
        file: PathBuf,
    },
    Repl,
}

fn report(errors: &[Diagnostic], file: &str, source: &str, color: Color) {
    let color = matches!(color, Color::Always)
        || matches!(color, Color::Auto) && io::stderr().is_terminal();
    for error in errors {
        let _ = error.render(file, source, color, io::stderr().lock());
    }
}

fn process(
    source: &str,
    file: &str,
    cli: &Cli,
    engine: Option<Engine>,
    dump: Option<Pass>,
) -> bool {
    if matches!(dump, Some(Pass::Tokens)) {
        return match lexer::lex(source) {
            Ok(tokens) => {
                for (a, token, b) in tokens {
                    println!("{a}..{b}: {token:?}");
                }
                true
            }
            Err(errors) => {
                report(&errors, file, source, cli.color);
                false
            }
        };
    }
    let expr = match parse(source) {
        Ok(expr) => expr,
        Err(e) => {
            report(&e, file, source, cli.color);
            return false;
        }
    };
    match dump {
        Some(Pass::Parse) => {
            println!("{expr}");
            return true;
        }
        Some(Pass::Debruijn) => {
            println!("{:?}", names::debruijn(&expr, &mut vec![]));
            return true;
        }
        _ => {}
    }
    if let Err(e) = eval::check(&expr) {
        report(&e, file, source, cli.color);
        return false;
    }
    let inferred = if matches!(dump, Some(Pass::TypesW)) {
        ch06_poly::algorithm_w::check(&expr)
    } else {
        types::check(&expr)
    };
    let ty = match inferred {
        Ok(ty) => ty,
        Err(e) => {
            report(&[e], file, source, cli.color);
            return false;
        }
    };
    if matches!(dump, Some(Pass::Types | Pass::TypesW)) {
        println!("{ty}");
        return true;
    }
    let Some(engine) = engine else {
        return true;
    };
    let mut limit = cli.fuel;
    // Tree evaluators need host stack; the iterative machine arrives in Chapter 5.
    let result = std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(move || match engine {
            Engine::Core => eval::evaluate(&expr, &vec![], &mut limit)
                .map(|v| v.to_string()),
            Engine::Cek => {
                cek::evaluate(&expr, limit).map(|r| r.value.to_string())
            }
            Engine::Cbn => {
                cbn::evaluate(&expr, &mut limit).map(|e| e.to_string())
            }
            Engine::Normal => {
                normal::reduce(expr, limit, |_| {}).map(|e| e.to_string())
            }
        })
        .expect("evaluator thread")
        .join()
        .expect("evaluator panic");
    match result {
        Ok(value) => {
            println!("{value}");
            true
        }
        Err(e) => {
            eprintln!("fun: {e}");
            false
        }
    }
}

fn repl(cli: &Cli) -> Result<(), Box<dyn std::error::Error>> {
    let mut editor = DefaultEditor::new()?;
    println!("Poly: :quit, :type EXPR, :parse EXPR, :trace EXPR.");
    loop {
        match editor.readline("poly> ") {
            Ok(line) if line.trim() == ":quit" => break,
            Ok(line) => {
                if line.trim().is_empty() {
                    continue;
                }
                editor.add_history_entry(&line)?;
                if let Some(source) = line.strip_prefix(":trace ") {
                    match parse(source) {
                        Ok(expr) => {
                            if let Err(e) = types::check(&expr) {
                                report(&[e], "<repl>", source, cli.color);
                                continue;
                            }
                            if let Err(e) =
                                normal::reduce(expr, cli.fuel, |e| {
                                    println!("{e}")
                                })
                            {
                                eprintln!("fun: {e}");
                            }
                        }
                        Err(e) => report(&e, "<repl>", source, cli.color),
                    }
                } else if let Some(source) = line.strip_prefix(":type ") {
                    process(source, "<repl>", cli, None, Some(Pass::Types));
                } else if let Some(source) = line.strip_prefix(":parse ") {
                    process(source, "<repl>", cli, None, Some(Pass::Parse));
                } else {
                    process(
                        &line,
                        "<repl>",
                        cli,
                        Some(Engine::Cek),
                        cli.dump,
                    );
                }
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break,
            Err(e) => return Err(e.into()),
        }
    }
    Ok(())
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    let (path, engine, dump) = match &cli.command {
        Command::Check { file } => (file, None, cli.dump),
        Command::Run { file, engine } => (file, Some(*engine), cli.dump),
        Command::Dump { file, pass } => (file, None, Some(*pass)),
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
    let success = match fs::read_to_string(path) {
        Ok(source) => process(
            &source,
            &path.display().to_string(),
            &cli,
            engine,
            dump,
        ),
        Err(e) => {
            eprintln!("fun: {e}");
            false
        }
    };
    if success {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
