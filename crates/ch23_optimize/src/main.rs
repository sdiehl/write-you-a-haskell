// The batch driver stops at pass boundaries and keeps IO outside compiler passes.
use ch23_optimize::{
    elaborate, eval, lexer, parse,
    session::{Options, Session},
    source::FileId,
    typecheck,
};
use clap::{Parser, Subcommand, ValueEnum};

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
    Match,
    Erase,
    Anf,
    Reach,
    Flat,
    Llvm,
    LlvmOpt,
}

#[derive(Clone, Copy, ValueEnum)]
enum Color {
    Auto,
    Always,
    Never,
}

#[derive(Clone, Copy, ValueEnum)]
enum Engine {
    Jit,
    Core,
    Anf,
    Flat,
}

#[derive(Parser)]
#[command(name = "fun-ch23", about = "Chapter 23: Optimization")]
struct Cli {
    #[command(subcommand)]
    command: Command,
    #[arg(long, global = true)]
    lint: bool,
    #[arg(long, global = true)]
    print_value: bool,
    #[arg(long, global = true)]
    print_steps: bool,
    #[arg(long, global = true)]
    no_prelude: bool,
    #[arg(long, global = true, value_enum, default_value = "auto")]
    color: Color,
    #[arg(long, global = true, value_enum)]
    dump: Option<Pass>,
    #[arg(long, global = true)]
    no_simplify: bool,
    #[arg(long, global = true, default_value_t = usize::MAX)]
    fuel: usize,
    #[arg(short = 'O', global = true, default_value_t = 2, value_parser = clap::value_parser!(u8).range(0..=3))]
    optimization: u8,
}

#[derive(Subcommand)]
enum Command {
    Build {
        file: PathBuf,
        #[arg(short = 'o', long)]
        output: PathBuf,
    },
    Check {
        file: PathBuf,
    },
    Run {
        file: PathBuf,
        #[arg(long, value_enum, default_value = "flat")]
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

struct BackendOptions<'a> {
    engine: Engine,
    simplify: bool,
    print_steps: bool,
    output: Option<&'a std::path::Path>,
    optimization: u8,
}

fn compile(
    sess: &mut Session,
    file: FileId,
    pass: Option<Pass>,
    run: bool,
    fuel: usize,
    backend: BackendOptions<'_>,
) -> Result<(), ch23_optimize::session::Stop> {
    let BackendOptions {
        engine,
        simplify,
        print_steps,
        output,
        optimization,
    } = backend;
    if matches!(pass, Some(Pass::Tokens | Pass::Layout)) {
        let tokens = if matches!(pass, Some(Pass::Layout)) {
            ch23_optimize::layout::layout(sess, file)?
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
        ch23_optimize::fixity::resolve(sess, program)?
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
    let renamed = ch23_optimize::rename::rename(sess, program)?;
    if matches!(pass, Some(Pass::Rename)) {
        for group in renamed.groups {
            println!("{group}");
        }
        return Ok(());
    }
    let syn = ch23_optimize::desugar::desugar(sess, renamed)?;
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
        ch23_optimize::core::lint(&core)
            .expect("internal error: Core lint");
    }
    if matches!(pass, Some(Pass::Core)) {
        for group in core.groups {
            println!("{group}");
        }
        return Ok(());
    }
    let core = ch23_optimize::match_compile::compile(sess, core)?;
    if sess.options.lint {
        ch23_optimize::core::lint(&core)
            .expect("internal error: match lint");
    }
    if matches!(pass, Some(Pass::Match)) {
        for group in &core.groups {
            println!("{group}")
        }
        return Ok(());
    }
    let wants_anf = matches!(
        pass,
        Some(
            Pass::Erase
                | Pass::Anf
                | Pass::Reach
                | Pass::Flat
                | Pass::Llvm
                | Pass::LlvmOpt
        )
    ) || output.is_some()
        || (run
            && matches!(engine, Engine::Anf | Engine::Flat | Engine::Jit));
    let mut core = Some(core);
    let anf = if wants_anf {
        let erased = ch23_optimize::erase::erase(
            core.take().expect("Core before erasure"),
        );
        if matches!(pass, Some(Pass::Erase)) {
            for (v, e) in erased.globals {
                println!("{v} = {e}");
                ch23_optimize::erase::discard(e);
            }
            return Ok(());
        }
        let mut program = ch23_optimize::anf::convert(sess, erased);
        if simplify {
            ch23_optimize::anf_simplify::simplify(sess, &mut program);
        }
        if sess.options.lint {
            ch23_optimize::anf_lint::lint(&program)
                .expect("internal error: ANF lint");
        }
        let roots = program
            .globals
            .iter()
            .filter(|(v, _)| v.text == "main")
            .map(|(v, _)| v.clone())
            .collect::<Vec<_>>();
        let reach = ch23_optimize::reach::analyze(&program, &roots);
        if matches!(pass, Some(Pass::Reach)) {
            print!(
                "{}",
                ch23_optimize::reach::display(
                    &reach,
                    program.globals.len()
                )
            );
            return Ok(());
        }
        ch23_optimize::reach::prune(&mut program, &reach);
        if matches!(pass, Some(Pass::Anf)) {
            for (v, e) in &program.globals {
                println!(
                    "{} = {}",
                    ch23_optimize::dump::binder(v, &program.reps),
                    ch23_optimize::dump::anf(e, &program.reps)
                );
            }
            return Ok(());
        }
        Some(program)
    } else {
        None
    };
    let (anf, flat) =
        if matches!(pass, Some(Pass::Flat | Pass::Llvm | Pass::LlvmOpt))
            || output.is_some()
            || (run && matches!(engine, Engine::Flat | Engine::Jit))
        {
            let program = ch23_optimize::closure::convert(
                sess,
                anf.expect("ANF requested"),
            );
            if sess.options.lint {
                ch23_optimize::flat_lint::lint(&program)
                    .expect("internal error: Flat lint");
            }
            if matches!(pass, Some(Pass::Flat)) {
                for f in &program.funcs {
                    println!(
                        "{} {} env={:?} params=({}) = {{\n{}\n}}",
                        f.name,
                        f.source,
                        f.env,
                        ch23_optimize::dump::binders(
                            &f.params,
                            &program.reps
                        ),
                        ch23_optimize::dump::flat(&f.body, &program.reps)
                    );
                }
                for g in &program.globals {
                    println!(
                        "{} = {}",
                        ch23_optimize::dump::binder(&g.name, &program.reps),
                        ch23_optimize::dump::flat(&g.body, &program.reps)
                    );
                }
                return Ok(());
            }
            (None, Some(program))
        } else {
            (anf, None)
        };
    if matches!(pass, Some(Pass::Llvm | Pass::LlvmOpt)) || output.is_some()
    {
        let program = flat.as_ref().expect("Flat requested for LLVM");
        let result = (|| -> Result<(), String> {
            if matches!(pass, Some(Pass::Llvm | Pass::LlvmOpt)) {
                let context = inkwell::context::Context::create();
                let backend = ch23_optimize::llvm::Backend::new(
                    &context,
                    program,
                    optimization,
                )
                .map_err(|e| e.to_string())?;
                if matches!(pass, Some(Pass::LlvmOpt)) {
                    backend
                        .optimize(optimization)
                        .map_err(|e| e.to_string())?;
                }
                print!("{}", backend.ir());
            } else if let Some(output) = output {
                ch23_optimize::native::build(
                    program,
                    output,
                    optimization,
                )?;
            }
            Ok(())
        })();
        return result.map_err(|e| {
            eprintln!("fun: {e}");
            ch23_optimize::session::Stop
        });
    }
    if !run {
        return Ok(());
    }
    if matches!(engine, Engine::Jit) {
        if !sess.options.require_io {
            eprintln!(
                "fun: --print-value requires an interpreter; use print in an IO program for JIT"
            );
            return Err(ch23_optimize::session::Stop);
        }
        let context = inkwell::context::Context::create();
        return ch23_optimize::jit::Jit::new(&context)
            .run(flat.as_ref().expect("Flat for JIT"))
            .map_err(|e| {
                eprintln!("fun: {e}");
                ch23_optimize::session::Stop
            });
    }
    let print_value = !sess.options.require_io;
    let result = if let Some(flat) = flat {
        ch23_optimize::flat_machine::run(&flat, fuel, !print_value)
            .map(|(value, _)| print_value.then(|| value.to_string()))
    } else if let Some(anf) = anf {
        ch23_optimize::anf_machine::run(&anf, fuel, !print_value).map(
            |(value, stats)| {
                if print_steps {
                    eprintln!("steps: {}", stats.steps);
                }
                print_value.then(|| value.to_string())
            },
        )
    } else if print_value {
        eval::run(core.as_ref().expect("Core engine"), fuel)
            .map(Some)
            .map_err(eval::Failure::Runtime)
    } else {
        eval::run_io(core.as_ref().expect("Core engine"), fuel)
            .map(|()| None)
    };
    match result {
        Ok(value) => {
            if let Some(value) = value {
                println!("{value}");
            }
            Ok(())
        }
        Err(eval::Failure::Exit(code)) => {
            sess.exit_code = code;
            Ok(())
        }
        Err(message) => {
            use std::io::Write;
            let _ = io::stdout().flush();
            eprintln!("fun: {message}");
            Err(ch23_optimize::session::Stop)
        }
    }
}

fn process(
    source: String,
    name: String,
    cli: &Cli,
    run: bool,
    pass: Option<Pass>,
) -> i32 {
    let mut sess = Session {
        options: Options {
            require_io: matches!(cli.command, Command::Build { .. })
                || (!cli.print_value && name != "<repl>"),
            lint: cli.lint,
            no_prelude: cli.no_prelude,
            dump: pass.map(|p| format!("{p:?}")),
        },
        ..Session::default()
    };
    let file = sess.sources.add(name, source);
    let engine = match cli.command {
        Command::Run { engine, .. } => engine,
        _ => Engine::Flat,
    };
    let success = compile(
        &mut sess,
        file,
        pass,
        run,
        cli.fuel,
        BackendOptions {
            engine,
            simplify: !cli.no_simplify,
            print_steps: cli.print_steps,
            optimization: cli.optimization,
            output: match &cli.command {
                Command::Build { output, .. } => Some(output.as_path()),
                _ => None,
            },
        },
    )
    .is_ok();
    let color = matches!(cli.color, Color::Always)
        || matches!(cli.color, Color::Auto) && io::stderr().is_terminal();
    for diagnostic in &sess.diagnostics {
        let _ =
            diagnostic.render(&sess.sources, color, io::stderr().lock());
    }
    use std::io::Write;
    let _ = io::stdout().flush();
    if success { sess.exit_code } else { 1 }
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
                        ch23_optimize::format::format(&mut sess, id)
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
        Command::Check { file } | Command::Build { file, .. } => {
            (file, false, cli.dump)
        }
        Command::Run { file, .. } => (file, true, cli.dump),
        Command::Dump { file, pass } => (file, false, Some(*pass)),
        Command::Repl => {
            if cli.no_prelude {
                eprintln!(
                    "fun: the REPL needs the Prelude for expression display and IO"
                );
                return ExitCode::FAILURE;
            }
            return match ch23_optimize::repl::run(cli.lint, cli.fuel) {
                Ok(()) => ExitCode::SUCCESS,
                Err(e) => {
                    eprintln!("fun: {e}");
                    ExitCode::FAILURE
                }
            };
        }
    };
    match fs::read_to_string(file) {
        Ok(source) => ExitCode::from(process(
            source,
            file.display().to_string(),
            &cli,
            run,
            pass,
        ) as u8),
        Err(e) => {
            eprintln!("fun: {e}");
            ExitCode::FAILURE
        }
    }
}
