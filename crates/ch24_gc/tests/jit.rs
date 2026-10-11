use ch24_gc::{
    anf, closure, elaborate, erase, jit::Jit, match_compile, parse,
    session::Session, typecheck,
};
use inkwell::context::Context;
use std::{path::Path, process::Command, time::Duration};

// Boehm's initializing thread must outlive every collection. Rust's parallel
// test workers are short-lived; each runtime test gets its own process lifetime.
fn isolated(name: &str) -> bool {
    if std::env::var("FUN_JIT_TEST").as_deref() == Ok(name) {
        return false;
    }
    let outcome = testkit::capture(
        Command::new(std::env::current_exe().unwrap())
            .args(["--exact", name, "--nocapture"])
            .env("FUN_JIT_TEST", name),
        Duration::from_secs(120),
    )
    .unwrap();
    assert_eq!(
        outcome.exit,
        0,
        "{name}: {}\n{}",
        String::from_utf8_lossy(&outcome.stdout),
        String::from_utf8_lossy(&outcome.stderr)
    );
    true
}

fn program(source: &str) -> ch24_gc::flat::Program {
    let mut sess = Session::default();
    sess.options.require_io = true;
    let file = sess.sources.add("jit.fun", source.into());
    let ast = parse::parse(&mut sess, file).unwrap();
    let typed = typecheck::check(&mut sess, ast).unwrap();
    let core = elaborate::elaborate(&mut sess, typed).unwrap();
    ch24_gc::core::lint(&core).unwrap();
    let core = match_compile::compile(&mut sess, core).unwrap();
    ch24_gc::core::lint(&core).unwrap();
    let anf = anf::convert(&mut sess, erase::erase(core));
    closure::convert(&mut sess, anf)
}

#[test]
fn imported_globals_keep_their_identity_and_code_across_collections() {
    if isolated(
        "imported_globals_keep_their_identity_and_code_across_collections",
    ) {
        return;
    }
    let context = Context::create();
    let mut jit = Jit::new(&context);
    let declarations =
        "make x = \\y -> x + y\nsaved = make 40\ntext = Just \"λ🙂\"\n";
    jit.run(&program(&format!("{declarations}main = pure ()")))
        .unwrap();
    let closure = jit.global_word("saved").unwrap();
    let text = jit.global_word("text").unwrap();
    for index in 0..6 {
        fun_rt::heap::collect();
        let source = format!(
            "{declarations}answer = saved 2\nmain = if answer == 42 && make 40 2 == 42 then pure () else error \"lost capture\"\nextra{index} = {index}"
        );
        jit.run(&program(&source)).unwrap();
        assert_eq!(jit.global_word("saved"), Some(closure));
        assert_eq!(jit.global_word("text"), Some(text));
        assert_eq!(
            jit.global_word("answer"),
            Some(fun_rt::layout::integer(42))
        );
        // SAFETY: text is a live, rooted Just String produced by this checked program;
        // its string constant and closure code are owned by the first retained engine.
        #[allow(unsafe_code)]
        unsafe {
            assert_eq!(
                fun_rt::heap::text(fun_rt::heap::field(text, 0)),
                "λ🙂"
            );
        }
    }
}

fn transcript(input: &str) -> testkit::Outcome {
    let mut command = Command::new(env!("CARGO_BIN_EXE_fun-ch24"));
    command.current_dir(env!("CARGO_MANIFEST_DIR"));
    command.args(["repl", "--lint", "--color=never"]);
    testkit::capture_with_input(
        &mut command,
        Duration::from_secs(120),
        Some(input.as_bytes()),
    )
    .unwrap()
}
#[test]
fn golden_repl_sessions() {
    let directory =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/repl");
    for name in ["definitions", "engines", "load", "recovery"] {
        let input =
            std::fs::read_to_string(directory.join(format!("{name}.in")))
                .unwrap();
        let outcome = transcript(&input);
        assert_eq!(
            outcome.exit,
            0,
            "{name}: {}\n{}",
            String::from_utf8_lossy(&outcome.stdout),
            String::from_utf8_lossy(&outcome.stderr)
        );
        assert_eq!(
            outcome.stdout,
            std::fs::read(directory.join(format!("{name}.out"))).unwrap(),
            "{name}"
        );
        assert_eq!(
            outcome.stderr,
            std::fs::read(directory.join(format!("{name}.err"))).unwrap(),
            "{name}"
        );
    }
}
#[test]
fn inspection_does_not_execute_and_includes_the_requested_ir() {
    let outcome = transcript(
        ":t error \"must not run\"\n:core 1 + 2\n:anf 1 + 2\n:llvm 1 + 2\n:quit\n",
    );
    assert_eq!(outcome.exit, 0);
    assert!(outcome.stderr.is_empty(), "{:?}", outcome);
    let text = String::from_utf8(outcome.stdout).unwrap();
    assert!(text.contains("error \"must not run\" :"));
    assert!(text.contains("define void @fun_entry()"));
    assert!(text.contains("rt_prim_putStr"));
    assert!(text.contains("$repl"));
}
#[test]
fn installed_binary_uses_the_same_driver() {
    let output = Command::new(env!("CARGO_BIN_EXE_fun"))
        .arg("--help")
        .output()
        .unwrap();
    assert!(output.status.success());
    assert!(
        String::from_utf8(output.stdout)
            .unwrap()
            .contains("Precise Garbage Collector")
    );
}

#[test]
fn jit_runs_deep_tail_calls_and_allocation_stress() {
    for name in
        ["acceptance", "mutual-tail", "allocation", "many-arguments"]
    {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join(format!("tests/programs/native/{name}.fun"));
        let outcome = testkit::capture(
            Command::new(env!("CARGO_BIN_EXE_fun-ch24"))
                .arg("run")
                .arg(&path)
                .args(["--engine=jit", "--lint"])
                .env("GC_MAXIMUM_HEAP_SIZE", "67108864"),
            Duration::from_secs(180),
        )
        .unwrap();
        assert_eq!(
            outcome.exit,
            0,
            "{name}: {}\n{}",
            String::from_utf8_lossy(&outcome.stdout),
            String::from_utf8_lossy(&outcome.stderr)
        );
        assert!(outcome.stderr.is_empty(), "{name}: {outcome:?}");
        assert_eq!(
            outcome.stdout,
            std::fs::read(path.with_extension("out")).unwrap(),
            "{name}"
        );
    }
}

#[test]
fn symbol_table_covers_all_primitives_without_duplicate_names() {
    let symbols = ch24_gc::jit::runtime_symbols();
    let names = symbols
        .iter()
        .map(|(name, address)| {
            assert_ne!(*address, 0);
            *name
        })
        .collect::<std::collections::BTreeSet<_>>();
    assert_eq!(names.len(), symbols.len());
    for primitive in fun_rt::ALL {
        assert!(
            names.contains(format!("rt_{}", primitive.name()).as_str())
        );
    }
}

#[test]
fn an_imported_binding_cannot_change_its_abi() {
    if isolated("an_imported_binding_cannot_change_its_abi") {
        return;
    }
    let context = Context::create();
    let mut jit = Jit::new(&context);
    jit.run(&program("old x = id x\nmain = pure ()")).unwrap();
    let changed = program("old x = id x\nid x = x + 1\nmain = pure ()");
    let error = jit.run(&changed).unwrap_err();
    assert!(
        error.contains("changes the accepted type of old"),
        "{error}"
    );
    jit.run(&program("old x = id x\nmain = if old True then pure () else error \"bad import\"")).unwrap();
    let mut other = Jit::new(&context);
    other
        .run(&program(
            "f :: Int -> Int -> Int\nf x y = x + y\nmain = pure ()",
        ))
        .unwrap();
    let changed = program(
        "f x = if x == 0 then (\\y -> y) else (\\y -> x + y)\nmain = pure ()",
    );
    let error = other.run(&changed).unwrap_err();
    assert!(error.contains("accepted arity of f"), "{error}");
}

#[test]
fn imported_type_fingerprints_use_a_fixed_rust_stack() {
    std::thread::Builder::new()
        .stack_size(512 * 1024)
        .spawn(|| {
            use ch24_gc::core::Type;
            let a = Type::Forall(
                1,
                Box::new(Type::arrow(Type::Var(1), Type::Var(1))),
            );
            let b = Type::Forall(
                7,
                Box::new(Type::arrow(Type::Var(7), Type::Var(7))),
            );
            assert_eq!(
                ch24_gc::core_type::fingerprint(&a),
                ch24_gc::core_type::fingerprint(&b)
            );
            let mut ty = Type::Int;
            for _ in 0..50_000 {
                ty = Type::arrow(Type::Bool, ty);
            }
            assert_eq!(ch24_gc::core_type::fingerprint(&ty).len(), 100_001);
            // The earlier chapter's boxed Type still has recursive Drop. Tear down this
            // artificial input explicitly so the test isolates the new traversal.
            let mut work = vec![ty];
            while let Some(ty) = work.pop() {
                if let Type::Arrow(a, b) = ty {
                    work.push(*a);
                    work.push(*b);
                }
            }
        })
        .unwrap()
        .join()
        .unwrap();
}
