//! REPL inputs are transactions: failed checking never changes the accepted source.
use crate::{
    ast, core,
    jit::Jit,
    session::{Options, Session},
    typecheck::TypedProgram,
};
use inkwell::context::Context;
use rustyline::{DefaultEditor, error::ReadlineError};
use std::io::{self, IsTerminal, Write};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// The JIT is the interactive default; interpreters remain useful for comparison.
pub enum Engine {
    Jit,
    Core,
    Anf,
    Flat,
}
#[derive(Clone)]
struct Input {
    name: String,
    source: String,
}
/// Source definitions and executable definitions commit together after a successful input.
pub struct Repl<'ctx> {
    inputs: Vec<Input>,
    jit: Jit<'ctx>,
    context: &'ctx Context,
    engine: Engine,
    lint: bool,
    fuel: usize,
}
impl<'ctx> Repl<'ctx> {
    pub fn new(context: &'ctx Context, lint: bool, fuel: usize) -> Self {
        Self {
            inputs: vec![],
            jit: Jit::new(context),
            context,
            engine: Engine::Jit,
            lint,
            fuel,
        }
    }
    fn reset(&mut self) {
        self.jit = Jit::new(self.context);
        self.inputs.clear();
    }
    /// Process one input. False requests a clean exit; diagnostics leave the session usable.
    pub fn line(&mut self, line: &str) -> Result<bool, String> {
        let line = line.trim();
        if line.is_empty() {
            return Ok(true);
        }
        let (command, rest) =
            line.split_once(char::is_whitespace).unwrap_or((line, ""));
        let rest = rest.trim();
        match command {
            ":q" | ":quit" => return Ok(false),
            ":reset" => {
                self.reset();
                println!("Reset.");
            }
            ":engine" => {
                if rest.is_empty() {
                    println!(
                        "{}",
                        format!("{:?}", self.engine).to_lowercase()
                    );
                } else {
                    let engine = match rest {
                        "jit" => Engine::Jit,
                        "core" => Engine::Core,
                        "anf" => Engine::Anf,
                        "flat" => Engine::Flat,
                        _ => {
                            return Err(
                                "expected :engine jit|core|anf|flat".into(),
                            );
                        }
                    };
                    // Switching engines rebuilds pure globals; source definitions survive.
                    self.jit = Jit::new(self.context);
                    self.engine = engine;
                    println!("Engine: {rest}");
                }
            }
            ":load" => {
                if rest.is_empty() {
                    return Err("expected :load FILE".into());
                }
                let input = Input {
                    name: rest.into(),
                    source: std::fs::read_to_string(rest)
                        .map_err(|e| e.to_string())?,
                };
                // Keep the old session until both checking and JIT preparation succeed.
                let mut next =
                    Self::new(self.context, self.lint, self.fuel);
                next.engine = self.engine;
                next.evaluate(std::slice::from_ref(&input), "pure ()")?;
                next.inputs.push(input);
                *self = next;
                println!("Loaded {rest}");
            }
            ":t" | ":type" => {
                let (_, typed) = prepare(&self.inputs, rest, false)?;
                println!("{rest} : {}", typed.main_description());
            }
            ":k" => {
                let (mut sess, typed) = prepare(&self.inputs, "()", false)?;
                let file = sess
                    .sources
                    .add("<kind>", format!("type ReplKind = {rest}"));
                let syntax = crate::parse::parse(&mut sess, file)
                    .map_err(|_| diagnostics(&sess))?;
                let alias =
                    syntax.aliases.first().ok_or("expected :k TYPE")?;
                let mut store = crate::kinds::Store::default();
                let expanded = crate::kinds::expand(
                    &alias.body,
                    &typed.surface.data.aliases,
                    alias.span,
                )
                .map_err(|e| e.message)?;
                let kind = store
                    .infer(
                        &expanded,
                        &typed.surface.data.kinds,
                        &mut std::collections::BTreeMap::new(),
                        false,
                        alias.span,
                    )
                    .map_err(|e| e.message)?;
                println!("{rest} : {}", store.finish(&kind));
            }
            ":core" | ":anf" | ":llvm" => self.dump(rest, command)?,
            ":help" => println!(
                "EXPR; declarations; :t EXPR; :k TYPE; :core/:anf/:llvm EXPR; :load FILE; :reset; :engine jit|core|anf|flat; :quit"
            ),
            _ if command.starts_with(':') => {
                return Err(format!(
                    "unknown command: {command}; use :help"
                ));
            }
            _ => {
                // Trial parsing distinguishes declarations from expressions without publishing diagnostics.
                let mut trial = Session::default();
                let file = trial.sources.add("<input>", line.into());
                if crate::parse::parse(&mut trial, file).is_ok() {
                    let mut candidate = self.inputs.clone();
                    candidate.push(Input {
                        name: format!("<input {}>", candidate.len() + 1),
                        source: line.into(),
                    });
                    self.evaluate(&candidate, "pure ()")?;
                    self.inputs = candidate;
                } else {
                    let inputs = self.inputs.clone();
                    let (_, typed) = prepare(&inputs, line, false)?;
                    let query = action_query(line, &typed.main_type);
                    self.evaluate(&inputs, &query)?;
                }
            }
        }
        Ok(true)
    }
    fn evaluate(
        &mut self,
        inputs: &[Input],
        query: &str,
    ) -> Result<(), String> {
        let (mut sess, typed) = prepare(inputs, query, true)?;
        let core = lower_core(&mut sess, typed, self.lint)?;
        if self.engine == Engine::Core {
            return crate::eval::run_io(&core, self.fuel)
                .map_err(|e| e.to_string());
        }
        let anf = lower_anf(&mut sess, core, self.lint)?;
        if self.engine == Engine::Anf {
            return crate::anf_machine::run(&anf, self.fuel, true)
                .map(|_| ())
                .map_err(|e| e.to_string());
        }
        let flat = crate::closure::convert(&mut sess, anf);
        crate::flat_lint::lint(&flat)?;
        if self.engine == Engine::Jit {
            match self.jit.run(&flat) {
                Ok(()) => return Ok(()),
                Err(e) if e.starts_with("JIT setup failed:") => {
                    eprintln!("fun: {e}; using flat interpreter");
                    self.engine = Engine::Flat;
                }
                Err(e) => return Err(e),
            }
        }
        crate::flat_machine::run(&flat, self.fuel, true)
            .map(|_| ())
            .map_err(|e| e.to_string())
    }
    fn dump(&self, expression: &str, pass: &str) -> Result<(), String> {
        let (mut sess, mut typed) =
            prepare(&self.inputs, expression, false)?;
        if pass == ":llvm" {
            let query = action_query(expression, &typed.main_type);
            (sess, typed) = prepare(&self.inputs, &query, true)?;
        }
        let core = lower_core(&mut sess, typed, self.lint)?;
        if pass == ":core" {
            for group in &core.groups {
                println!("{group}");
            }
            return Ok(());
        }
        let anf = lower_anf(&mut sess, core, self.lint)?;
        if pass == ":anf" {
            for (name, body) in &anf.globals {
                println!("{name} = {body}");
            }
            return Ok(());
        }
        let flat = crate::closure::convert(&mut sess, anf);
        let backend = crate::llvm::Backend::for_jit(
            self.context,
            &flat,
            Default::default(),
        )
        .map_err(|e| e.to_string())?;
        print!("{}", backend.ir());
        Ok(())
    }
}
/// Both display and IO sequencing are ordinary, type-checked Prelude operations.
fn action_query(expression: &str, ty: &crate::types::Type) -> String {
    if matches!(ty, crate::types::Type::Con(id, _) if id.name == "IO") {
        format!("({expression}) >> pure ()")
    } else {
        format!("print ({expression})")
    }
}
fn diagnostics(sess: &Session) -> String {
    let mut bytes = vec![];
    for diagnostic in &sess.diagnostics {
        let _ = diagnostic.render(&sess.sources, false, &mut bytes);
    }
    String::from_utf8_lossy(&bytes).trim_end().into()
}
fn append(to: &mut ast::Program, mut from: ast::Program) {
    to.bindings.append(&mut from.bindings);
    to.fixities.append(&mut from.fixities);
    to.data.append(&mut from.data);
    to.aliases.append(&mut from.aliases);
    to.classes.append(&mut from.classes);
    to.instances.append(&mut from.instances);
}
fn prepare(
    inputs: &[Input],
    query: &str,
    io: bool,
) -> Result<(Session, TypedProgram), String> {
    let mut sess = Session {
        options: Options {
            require_io: io,
            ..Options::default()
        },
        entry_name: Some("$repl".into()),
        ..Session::default()
    };
    let mut program = None;
    let mut later_files = vec![];
    for (index, input) in inputs.iter().enumerate() {
        let file =
            sess.sources.add(input.name.clone(), input.source.clone());
        if index > 0 {
            later_files.push(file);
        }
        let parsed = crate::parse::parse(&mut sess, file)
            .map_err(|_| diagnostics(&sess))?;
        if index > 0
            && parsed.fixities.iter().any(|f| {
                f.operators.iter().any(|op| {
                    !parsed.bindings.iter().any(|b| b.name == *op)
                })
            })
        {
            return Err("a REPL fixity must accompany its operator definition in the same input".into());
        }
        if let Some(program) = &mut program {
            append(program, parsed);
        } else {
            program = Some(parsed);
        }
    }
    let file = sess.sources.add("<query>", format!("main = {query}"));
    let mut entry = crate::parse::parse(&mut sess, file)
        .map_err(|_| diagnostics(&sess))?;
    entry.bindings[0].name = "$repl".into();
    let program = if let Some(mut program) = program {
        append(&mut program, entry);
        program
    } else {
        entry
    };
    let typed = crate::typecheck::check(&mut sess, program)
        .map_err(|_| diagnostics(&sess))?;
    // Retained code and replaying interpreters must agree on name resolution.
    // Put Prelude overrides in the initial declaration block or a loaded file.
    let prelude_names = typed
        .surface
        .groups
        .iter()
        .flat_map(|g| &g.binds)
        .filter(|b| Some(b.span.file) == sess.prelude_file)
        .map(|b| b.id.text.as_str())
        .collect::<std::collections::BTreeSet<_>>();
    for binding in typed.surface.groups.iter().flat_map(|g| &g.binds) {
        if later_files.contains(&binding.span.file)
            && prelude_names.contains(binding.id.text.as_str())
        {
            return Err(format!(
                "{} shadows the Prelude after earlier definitions; use :reset or :load",
                binding.id.text
            ));
        }
    }
    Ok((sess, typed))
}
fn lower_core(
    sess: &mut Session,
    typed: TypedProgram,
    lint: bool,
) -> Result<core::Program, String> {
    let core = crate::elaborate::elaborate(sess, typed)
        .map_err(|_| diagnostics(sess))?;
    if lint {
        core::lint(&core)?;
    }
    let core = crate::match_compile::compile(sess, core)
        .map_err(|_| diagnostics(sess))?;
    if lint {
        core::lint(&core)?;
    }
    Ok(core)
}
fn lower_anf(
    sess: &mut Session,
    core: core::Program,
    lint: bool,
) -> Result<crate::anf::Program, String> {
    let mut anf = crate::anf::convert(sess, crate::erase::erase(core));
    crate::anf_simplify::simplify(sess, &mut anf);
    let roots = anf
        .globals
        .iter()
        .map(|(id, _)| id.clone())
        .collect::<Vec<_>>();
    let reach = crate::reach::analyze(&anf, &roots);
    crate::reach::prune(&mut anf, &reach);
    if lint {
        crate::anf_lint::lint(&anf)?;
    }
    Ok(anf)
}
/// Terminal input uses rustyline; redirected input has deterministic, prompt-free output.
pub fn run(
    lint: bool,
    fuel: usize,
) -> Result<(), Box<dyn std::error::Error>> {
    let context = Context::create();
    let mut repl = Repl::new(&context, lint, fuel);
    let interactive = io::stdin().is_terminal();
    let mut editor = if interactive {
        Some(DefaultEditor::new()?)
    } else {
        None
    };
    if interactive {
        println!("Fun: JIT ready on first input. Type :help for commands.");
    }
    loop {
        let line = if let Some(editor) = &mut editor {
            match editor.readline("fun> ") {
                Ok(line) => {
                    editor.add_history_entry(&line)?;
                    line
                }
                Err(ReadlineError::Eof | ReadlineError::Interrupted) => {
                    break;
                }
                Err(e) => return Err(e.into()),
            }
        } else {
            let mut line = String::new();
            if io::stdin().read_line(&mut line)? == 0 {
                break;
            }
            line
        };
        match repl.line(&line) {
            Ok(false) => break,
            Ok(true) => {}
            Err(e) => eprintln!("fun: {e}"),
        }
        io::stdout().flush()?;
    }
    Ok(())
}
