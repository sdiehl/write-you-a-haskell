//! File driver and line-oriented REPL for the calculator.
use ch02_calc::{
    ast::Expr, diagnostic::Diagnostic, eval, lexer, parse, pratt,
};
use clap::{Parser, Subcommand, ValueEnum};
use rustyline::{DefaultEditor, error::ReadlineError};
use std::{
    fs,
    io::{self, IsTerminal},
    path::{Path, PathBuf},
    process::ExitCode,
};

#[derive(Clone, Copy, Default, ValueEnum)]
enum ParserChoice {
    #[default]
    Lalrpop,
    Pratt,
}
#[derive(Clone, Copy, ValueEnum)]
enum Pass {
    Tokens,
    Parse,
}
#[derive(Clone, Copy, ValueEnum)]
enum Engine {
    Core,
}
#[derive(Clone, Copy, Default, ValueEnum)]
enum Color {
    #[default]
    Auto,
    Always,
    Never,
}
#[derive(Parser)]
#[command(
    name = "fun-ch02",
    about = "Chapter 2: arithmetic and lexical bindings"
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
    #[arg(long, global = true, value_enum, default_value = "lalrpop")]
    parser: ParserChoice,
    #[arg(long, global = true, value_enum, default_value = "auto")]
    color: Color,
    /// Check the parsed tree before executing (always enabled in this chapter).
    #[arg(long, global = true)]
    lint: bool,
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
fn parsed(
    source: &str,
    parser: ParserChoice,
) -> Result<Expr, Vec<Diagnostic>> {
    match parser {
        ParserChoice::Lalrpop => parse(source),
        ParserChoice::Pratt => pratt::parse(source),
    }
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
    execute: bool,
    dump: Option<Pass>,
) -> bool {
    let result = if matches!(dump, Some(Pass::Tokens)) {
        lexer::lex(source).map(|tokens| {
            for (start, token, end) in tokens {
                println!("{start}..{end}: {token:?}");
            }
        })
    } else {
        parsed(source, cli.parser).and_then(|expr| {
            if matches!(dump, Some(Pass::Parse)) {
                println!("{expr}");
                return Ok(());
            }
            eval::check(&expr)?;
            if execute {
                match eval::evaluate(&expr, &mut Vec::new()) {
                    Ok(value) => println!("{value}"),
                    Err(message) => {
                        return Err(vec![Diagnostic::new(
                            "runtime", message, expr.span,
                        )]);
                    }
                }
            }
            Ok(())
        })
    };
    match result {
        Ok(()) => true,
        Err(errors) => {
            if errors[0].code == "runtime" {
                eprintln!("fun: {}", errors[0].message);
            } else {
                report(&errors, file, source, cli.color);
            }
            false
        }
    }
}
fn file(path: &Path, cli: &Cli, execute: bool, dump: Option<Pass>) -> bool {
    match fs::read_to_string(path) {
        Ok(source) => process(
            &source,
            &path.display().to_string(),
            cli,
            execute,
            dump,
        ),
        Err(error) => {
            eprintln!("fun: {}: {error}", path.display());
            false
        }
    }
}
fn repl(cli: &Cli) -> Result<(), Box<dyn std::error::Error>> {
    let mut editor = DefaultEditor::new()?;
    println!("Calc: :quit to exit, :parse EXPR to inspect the AST.");
    loop {
        match editor.readline("calc> ") {
            Ok(line) if line.trim() == ":quit" => break,
            Ok(line) => {
                if line.trim().is_empty() {
                    continue;
                }
                editor.add_history_entry(&line)?;
                if let Some(source) = line.strip_prefix(":parse ") {
                    process(
                        source,
                        "<repl>",
                        cli,
                        false,
                        Some(Pass::Parse),
                    );
                } else {
                    process(&line, "<repl>", cli, true, cli.dump);
                }
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break,
            Err(error) => return Err(error.into()),
        }
    }
    Ok(())
}
fn main() -> ExitCode {
    let cli = Cli::parse();
    let success = match &cli.command {
        Command::Check { file: path } => file(path, &cli, false, cli.dump),
        Command::Run { file: path, .. } => file(path, &cli, true, cli.dump),
        Command::Dump { file: path, pass } => {
            file(path, &cli, false, Some(*pass))
        }
        Command::Repl => match repl(&cli) {
            Ok(()) => true,
            Err(e) => {
                eprintln!("fun: {e}");
                false
            }
        },
    };
    if success {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
