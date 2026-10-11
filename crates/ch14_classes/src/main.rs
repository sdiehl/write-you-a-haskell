//! A small driver stops at pass boundaries; only this module reads files or renders errors.
use ch14_classes::{
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
    Layout,
    Fixity,
    Rename,
    Syn,
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
#[command(
    name = "fun-ch14",
    about = "Chapter 12: nominal records and field constraints"
)]
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
    Fmt {
        file: PathBuf,
        #[arg(long)]
        check: bool,
    },
    Repl,
}
fn compile(
    sess: &mut Session,
    file: FileId,
    pass: Option<Pass>,
    run: bool,
    fuel: usize,
) -> Result<(), ch14_classes::session::Stop> {
    if matches!(pass, Some(Pass::Tokens | Pass::Layout)) {
        let tokens = if matches!(pass, Some(Pass::Layout)) {
            ch14_classes::layout::layout(sess, file)?
        } else {
            lexer::lex(sess, file)?
        };
        for (l, t, r) in tokens {
            println!("{l}..{r}: {t:?}");
        }
        return Ok(());
    }
    let program = parse::parse(sess, file)?;
    let program = if matches!(pass, Some(Pass::Fixity)) {
        ch14_classes::fixity::resolve(sess, program)?
    } else {
        program
    };
    if matches!(pass, Some(Pass::Parse | Pass::Fixity)) {
        println!("{{");
        for d in &program.data {
            println!("  {d};");
        }
        for alias in &program.aliases {
            println!("  {alias};");
        }
        for class in &program.classes {
            println!("  {class};");
        }
        for instance in &program.instances {
            println!("  {instance};");
        }
        for b in program.bindings {
            println!("  {} = {};", b.name, b.rhs);
        }
        println!("}}");
        return Ok(());
    }
    let renamed = ch14_classes::rename::rename(sess, program)?;
    if matches!(pass, Some(Pass::Rename)) {
        for group in renamed.groups {
            println!("{group}");
        }
        return Ok(());
    }
    let syn = ch14_classes::desugar::desugar(sess, renamed)?;
    if matches!(pass, Some(Pass::Syn)) {
        for group in syn.groups {
            println!("{group}");
        }
        return Ok(());
    }
    let typed = typecheck::check_syn(sess, syn)?;
    if matches!(pass, Some(Pass::Types)) {
        println!("main : {}", typed.main_description());
        return Ok(());
    }
    let core = elaborate::elaborate(sess, typed)?;
    if sess.options.lint {
        ch14_classes::core::lint(&core).expect("internal error: Core lint");
    }
    if matches!(pass, Some(Pass::Core)) {
        for group in core.groups {
            println!("{group}");
        }
        return Ok(());
    }
    if !run {
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
            Err(ch14_classes::session::Stop)
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
        Command::Fmt { file, check } => {
            let mut sess = Session::default();
            let result =
                (|| -> Result<bool, Box<dyn std::error::Error>> {
                    let source = fs::read_to_string(file)?;
                    let id = sess
                        .sources
                        .add(file.display().to_string(), source.clone());
                    let Ok(formatted) =
                        ch14_classes::format::format(&mut sess, id)
                    else {
                        return Ok(false);
                    };
                    if *check {
                        return Ok(source == formatted);
                    }
                    fs::write(file, formatted)?;
                    Ok(true)
                })();
            for diagnostic in &sess.diagnostics {
                let _ = diagnostic.render(
                    &sess.sources,
                    false,
                    io::stderr().lock(),
                );
            }
            return match result {
                Ok(true) => ExitCode::SUCCESS,
                Ok(false) => ExitCode::FAILURE,
                Err(e) => {
                    eprintln!("fun: {e}");
                    ExitCode::FAILURE
                }
            };
        }
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
