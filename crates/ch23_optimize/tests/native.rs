use ch23_optimize::{
    core,
    flat::{self, Atom, Flat},
    llvm::Backend,
    names::Id,
    session::Session,
};
use inkwell::context::Context;
use std::{
    collections::BTreeMap, path::PathBuf, process::Command, rc::Rc,
    time::Duration,
};

fn program(
    sess: &mut Session,
    body: flat::Body,
    name: Id,
) -> flat::Program {
    let unit = sess.fresh_id("unit");
    // Literal lets in the depth fixture have explicit integer evidence.
    // Other fixtures supply their declared parameter/result evidence below.
    let mut reps = BTreeMap::from([
        (unit.clone(), ch23_optimize::rep::Rep::Word),
        (name.clone(), ch23_optimize::rep::Rep::Word),
    ]);
    let mut work = vec![body.as_ref()];
    while let Some(node) = work.pop() {
        match node {
            Flat::Let(
                v,
                flat::Rhs::Atom(Atom::Lit(core::Lit::Int(_))),
                rest,
            ) => {
                reps.insert(v.clone(), ch23_optimize::rep::Rep::Int);
                work.push(rest);
            }
            Flat::Join(_, _, b, r) => {
                work.push(b);
                work.push(r);
            }
            Flat::Case(_, alts) => {
                for (_, _, b) in alts {
                    work.push(b);
                }
            }
            Flat::Let(_, _, rest) => work.push(rest),
            _ => {}
        }
    }
    flat::Program {
        reps,
        funcs: vec![flat::Func {
            name: flat::FuncId(0),
            source: name.clone(),
            env: None,
            params: vec![unit],
            body,
            captures: vec![],
        }],
        globals: vec![flat::GlobalInit {
            name: name.clone(),
            function: Some(flat::FuncId(0)),
            body: Rc::new(Flat::Ret(Atom::Global(name.clone()))),
        }],
        metadata: core::Program {
            groups: vec![],
            main: core::Id {
                name,
                ty: core::Type::Int,
            },
            data_types: vec![],
            type_kinds: BTreeMap::new(),
            constructors: vec![],
            locations: vec![],
            global_types: Default::default(),
        },
    }
}

#[test]
fn deeply_nested_ir_uses_a_worklist_including_lint_and_drop() {
    std::thread::Builder::new()
        .stack_size(512 * 1024)
        .spawn(|| {
            let mut sess = Session::default();
            let name = sess.fresh_id("main");
            let mut body = Rc::new(Flat::Ret(Atom::Lit(core::Lit::Int(0))));
            // Cases and joins exercise both branching worklists; lets exercise a long spine.
            for _ in 0..5000 {
                let j = sess.fresh_id("join");
                body = Rc::new(Flat::Join(
                    j.clone(),
                    vec![],
                    body,
                    Rc::new(Flat::Case(
                        Atom::Lit(core::Lit::Int(0)),
                        vec![(
                            core::AltCon::Default,
                            vec![],
                            Rc::new(Flat::Jump(j, vec![])),
                        )],
                    )),
                ));
            }
            for _ in 0..50000 {
                body = Rc::new(Flat::Let(
                    sess.fresh_id("local"),
                    flat::Rhs::Atom(Atom::Lit(core::Lit::Int(0))),
                    body,
                ));
            }
            let program = program(&mut sess, body, name);
            assert!(program.funcs[0].body.to_string().contains("jump"));
            let ctx = Context::create();
            let backend = Backend::new(&ctx, &program, 0).unwrap();
            assert!(backend.ir().contains("define internal tailcc i64"));
            // Normal destruction, including the owned Flat tree, occurs on this small stack.
        })
        .unwrap()
        .join()
        .unwrap();
}

fn source(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/programs/native")
        .join(format!("{name}.fun"))
}

fn build(name: &str, level: u8, out: &std::path::Path) {
    let result = testkit::capture(
        Command::new(env!("CARGO_BIN_EXE_fun-ch23"))
            .arg("build")
            .arg(source(name))
            .arg("-o")
            .arg(out)
            .arg(format!("-O{level}"))
            .arg("--lint"),
        Duration::from_secs(120),
    )
    .unwrap();
    assert_eq!(result.exit, 0, "{result:?}");
}

#[test]
fn native_tail_calls_and_gc_survive_a_small_stack() {
    let dir = std::env::temp_dir()
        .join(format!("fun-native-deep-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    for name in ["acceptance", "mutual-tail", "allocation"] {
        let exe = dir.join(name);
        build(name, 2, &exe);
        let result = testkit::capture(
            Command::new("sh")
                .args(["-c", "ulimit -s 512; exec \"$1\"", "fun-test"])
                .arg(&exe)
                .env("GC_MAXIMUM_HEAP_SIZE", "67108864"),
            Duration::from_secs(180),
        )
        .unwrap();
        assert_eq!(result.exit, 0, "{name}: {result:?}");
        assert_eq!(
            result.stdout,
            std::fs::read(source(name).with_extension("out")).unwrap(),
            "{name}"
        );
        assert!(result.stderr.is_empty(), "{name}: {result:?}");
    }
    std::fs::remove_dir_all(dir).unwrap();
}

#[test]
fn all_optimization_levels_preserve_representations_and_errors() {
    let dir = std::env::temp_dir()
        .join(format!("fun-native-levels-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    for level in 0..=3 {
        for name in [
            "representation",
            "argument-order",
            "many-arguments",
            "global-order",
        ] {
            let exe = dir.join(format!("{name}-{level}"));
            build(name, level, &exe);
            let result = testkit::capture(
                &mut Command::new(exe),
                Duration::from_secs(30),
            )
            .unwrap();
            testkit::Fixture::read(source(name))
                .unwrap()
                .compare(&result)
                .unwrap();
        }
    }
    std::fs::remove_dir_all(dir).unwrap();
}

#[test]
fn llvm_lowering_snapshots() {
    use ch23_optimize::{
        ast::Op,
        primitives::Prim,
        source::{FileId, Span},
    };
    let span = Span::new(FileId(0), 0, 1);
    let integer = |n| Atom::Lit(core::Lit::Int(n));
    for name in [
        "identity",
        "integer",
        "division",
        "double",
        "string",
        "known-tail",
        "unknown-wide",
        "join",
        "switch",
        "failure",
    ] {
        let mut sess = Session::default();
        let main = sess.fresh_id("main");
        let x = sess.fresh_id("x");
        let y = sess.fresh_id("y");
        let j = sess.fresh_id("join");
        let ret = |a| Rc::new(Flat::Ret(a));
        let bind = |rhs| {
            Rc::new(Flat::Let(y.clone(), rhs, ret(Atom::Var(y.clone()))))
        };
        let body = match name {
            "identity" => ret(Atom::Var(x.clone())),
            "integer" => bind(flat::Rhs::Prim(
                Op::Add,
                vec![Atom::Var(x.clone()), integer(1)],
                span,
            )),
            "division" => bind(flat::Rhs::Prim(
                Op::Div,
                vec![Atom::Var(x.clone()), integer(-3)],
                span,
            )),
            "double" => bind(flat::Rhs::Prim(
                Op::Runtime(Prim::DblMul),
                vec![
                    Atom::Var(x.clone()),
                    Atom::Lit(core::Lit::Double(2.5f64.to_bits())),
                ],
                span,
            )),
            "string" => ret(Atom::Lit(core::Lit::String("λ\0🦀".into()))),
            "known-tail" => Rc::new(Flat::TailCallKnown(
                flat::FuncId(0),
                None,
                vec![Atom::Var(x.clone())],
            )),
            "unknown-wide" => Rc::new(Flat::TailCallUnknown(
                Atom::Var(x.clone()),
                (1..=5).map(integer).collect(),
            )),
            "join" => Rc::new(Flat::Join(
                j.clone(),
                vec![y.clone()],
                ret(Atom::Var(y.clone())),
                Rc::new(Flat::Case(
                    Atom::Var(x.clone()),
                    vec![
                        (
                            core::AltCon::Lit(core::Lit::Int(0)),
                            vec![],
                            Rc::new(Flat::Jump(
                                j.clone(),
                                vec![integer(10)],
                            )),
                        ),
                        (
                            core::AltCon::Default,
                            vec![],
                            Rc::new(Flat::Jump(j, vec![integer(20)])),
                        ),
                    ],
                )),
            )),
            "switch" => Rc::new(Flat::Case(
                Atom::Var(x.clone()),
                vec![
                    (
                        core::AltCon::Lit(core::Lit::Char('λ')),
                        vec![],
                        ret(integer(1)),
                    ),
                    (core::AltCon::Default, vec![], ret(integer(0))),
                ],
            )),
            "failure" => Rc::new(Flat::Fail(span)),
            _ => unreachable!(),
        };
        let mut p = program(&mut sess, body, main);
        let evidence = match name {
            "integer" | "division" | "join" => ch23_optimize::rep::Rep::Int,
            "double" => ch23_optimize::rep::Rep::Double,
            _ => ch23_optimize::rep::Rep::Word,
        };
        p.reps.insert(x.clone(), evidence);
        p.reps.insert(y, evidence);
        p.funcs[0].params = vec![x];
        p.metadata.locations.push((span, "example.fun:3:7".into()));
        let ctx = Context::create();
        let backend = Backend::new(&ctx, &p, 2).unwrap();
        // Normalize host headers and Inkwell's Mach-O section prefix, retaining all instructions.
        let text = backend
            .ir()
            .lines()
            .filter(|s| !s.starts_with("target "))
            .collect::<Vec<_>>()
            .join("\n")
            .replace(
                "section \",llvm.metadata\"",
                "section \"llvm.metadata\"",
            );
        insta::assert_snapshot!(format!("llvm-{name}"), text);
        backend.optimize(2).unwrap();
    }
}

#[test]
fn llvm_verifier_reports_a_broken_block() {
    let mut sess = Session::default();
    let main = sess.fresh_id("main");
    let p = program(
        &mut sess,
        Rc::new(Flat::Ret(Atom::Lit(core::Lit::Int(0)))),
        main,
    );
    let ctx = Context::create();
    let backend = Backend::new(&ctx, &p, 0).unwrap();
    let f = backend.module.add_function(
        "broken",
        ctx.void_type().fn_type(&[], false),
        None,
    );
    ctx.append_basic_block(f, "unterminated");
    let error = backend.verify().unwrap_err().to_string();
    assert!(
        error.contains("LLVM verification") && error.contains("terminator"),
        "{error}"
    );
}

#[test]
fn known_tail_calls_work_without_optimization() {
    let dir = std::env::temp_dir()
        .join(format!("fun-native-o0-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let exe = dir.join("mutual");
    build("mutual-tail", 0, &exe);
    let result = testkit::capture(
        Command::new("sh")
            .args(["-c", "ulimit -s 512; exec \"$1\"", "fun-test"])
            .arg(&exe),
        Duration::from_secs(120),
    )
    .unwrap();
    assert_eq!(result.exit, 0, "{result:?}");
    assert_eq!(result.stdout, b"True\n");
    assert!(result.stderr.is_empty());
    std::fs::remove_dir_all(dir).unwrap();
}

#[test]
fn failed_link_preserves_the_destination() {
    let dir = std::env::temp_dir()
        .join(format!("fun-native-failure-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let exe = dir.join("existing program");
    std::fs::write(&exe, b"keep this output").unwrap();
    let result = testkit::capture(
        Command::new(env!("CARGO_BIN_EXE_fun-ch23"))
            .arg("build")
            .arg(source("many-arguments"))
            .arg("-o")
            .arg(&exe)
            .arg("-O0")
            .env("CC", "false"),
        Duration::from_secs(120),
    )
    .unwrap();
    assert_eq!(result.exit, 1, "{result:?}");
    assert!(
        String::from_utf8_lossy(&result.stderr)
            .contains("native link failed")
    );
    assert_eq!(std::fs::read(&exe).unwrap(), b"keep this output");
    assert_eq!(std::fs::read_dir(&dir).unwrap().count(), 1);
    std::fs::remove_dir_all(dir).unwrap();
}

#[test]
fn exported_wide_calls_preserve_the_c_and_tailcc_frames() {
    use ch23_optimize::{
        anf, closure, elaborate, erase, match_compile, parse, typecheck,
    };
    let mut sess = Session::default();
    sess.options.require_io = true;
    let file = sess.sources.add(
        "wide.fun",
        std::fs::read_to_string(source("many-arguments")).unwrap(),
    );
    let ast = parse::parse(&mut sess, file).unwrap();
    let typed = typecheck::check(&mut sess, ast).unwrap();
    let core = elaborate::elaborate(&mut sess, typed).unwrap();
    let core = match_compile::compile(&mut sess, core).unwrap();
    let anf = anf::convert(&mut sess, erase::erase(core));
    let flat = closure::convert(&mut sess, anf);
    let context = Context::create();
    let backend = Backend::new(&context, &flat, 1).unwrap();
    // Preserve externally callable entries, as a later JIT module needs them.
    // Otherwise inlining can hide the mismatched stack-cleanup boundary.
    for function in backend.module.get_functions() {
        if function.get_name().to_bytes().starts_with(b"fun.f") {
            function.set_linkage(inkwell::module::Linkage::External);
        }
    }
    backend.optimize(1).unwrap();
    let dir = std::env::temp_dir()
        .join(format!("fun-exported-22-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let object = dir.join("wide.o");
    let executable = dir.join("wide");
    backend.object(&object).unwrap();
    let archive = std::path::Path::new(env!("CARGO_BIN_EXE_fun-ch23"))
        .parent()
        .unwrap()
        .join("libfun_rt_23.a");
    let mut linker =
        Command::new(std::env::var_os("CC").unwrap_or_else(|| "cc".into()));
    linker.arg(object).arg(archive);
    if let Some(gc) = std::env::var_os("GC_LIB_DIR") {
        linker.arg("-L").arg(gc);
    } else if cfg!(target_os = "macos") {
        for directory in ["/opt/homebrew/lib", "/usr/local/lib"] {
            if std::path::Path::new(directory).join("libgc.dylib").exists()
            {
                linker.arg("-L").arg(directory);
                break;
            }
        }
    }
    let linked = linker
        .args(env!("FUN_NATIVE_STATIC_LIBS").split_whitespace())
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        linked.status.success(),
        "{}",
        String::from_utf8_lossy(&linked.stderr)
    );
    let outcome = testkit::capture(
        &mut Command::new(executable),
        Duration::from_secs(30),
    )
    .unwrap();
    assert_eq!(outcome.exit, 0, "{outcome:?}");
    assert_eq!(outcome.stdout, b"55\n");
    assert!(outcome.stderr.is_empty(), "{outcome:?}");
    std::fs::remove_dir_all(dir).unwrap();
}

#[test]
fn arithmetic_chains_box_only_at_the_uniform_call_boundary() {
    use ch23_optimize::{
        ast::Op, primitives::Prim, rep::Rep, source::Span,
    };
    for floating in [false, true] {
        let mut sess = Session::default();
        let name =
            sess.fresh_id(if floating { "doubleLoop" } else { "intLoop" });
        let acc = sess.fresh_id("acc");
        let count = sess.fresh_id("count");
        let added = sess.fresh_id("added");
        let product = sess.fresh_id("product");
        let next = sess.fresh_id("next");
        let rep = if floating { Rep::Double } else { Rep::Int };
        let literal = |n: i64| {
            Atom::Lit(if floating {
                core::Lit::Double((n as f64).to_bits())
            } else {
                core::Lit::Int(n)
            })
        };
        let span = Span::new(ch23_optimize::source::FileId(0), 0, 0);
        let mut loop_body = Rc::new(Flat::TailCallKnown(
            flat::FuncId(0),
            None,
            vec![Atom::Var(product.clone()), Atom::Var(next.clone())],
        ));
        for (v, op, args) in [
            (
                next.clone(),
                Prim::IntSub,
                vec![
                    Atom::Var(count.clone()),
                    Atom::Lit(core::Lit::Int(1)),
                ],
            ),
            (
                product.clone(),
                if floating { Prim::DblMul } else { Prim::IntMul },
                vec![Atom::Var(added.clone()), literal(2)],
            ),
            (
                added.clone(),
                if floating { Prim::DblAdd } else { Prim::IntAdd },
                vec![Atom::Var(acc.clone()), literal(3)],
            ),
        ] {
            loop_body = Rc::new(Flat::Let(
                v,
                flat::Rhs::Prim(Op::Runtime(op), args, span),
                loop_body,
            ));
        }
        let body = Rc::new(Flat::Case(
            Atom::Var(count.clone()),
            vec![
                (
                    core::AltCon::Lit(core::Lit::Int(0)),
                    vec![],
                    Rc::new(Flat::Ret(Atom::Var(acc.clone()))),
                ),
                (core::AltCon::Default, vec![], loop_body),
            ],
        ));
        let mut program = program(
            &mut sess,
            Rc::new(Flat::Ret(Atom::Lit(core::Lit::Int(0)))),
            name,
        );
        program.funcs[0].params = vec![acc.clone(), count.clone()];
        program.funcs[0].body = body;
        program.reps.extend([
            (acc, rep),
            (count, Rep::Int),
            (added, rep),
            (product, rep),
            (next, Rep::Int),
        ]);
        let ctx = Context::create();
        let backend = Backend::new(&ctx, &program, 2).unwrap();
        // Keep the contract's region identifiable while still optimizing its loop.
        let noinline = ctx.create_enum_attribute(
            inkwell::attributes::Attribute::get_named_enum_kind_id(
                "noinline",
            ),
            0,
        );
        backend
            .module
            .get_function("fun.f0")
            .unwrap()
            .add_attribute(
                inkwell::attributes::AttributeLoc::Function,
                noinline,
            );
        backend.optimize(2).unwrap();
        let ir = backend.ir();
        let function = ir
            .split("define internal tailcc i64 @fun.f0(")
            .nth(1)
            .unwrap_or_else(|| panic!("loop function: {ir}"))
            .split("\n}")
            .next()
            .unwrap();
        let runtime_calls = function
            .lines()
            .filter(|l| l.contains("call ") && l.contains("@rt_"))
            .collect::<Vec<_>>();
        if floating {
            assert_eq!(runtime_calls.len(), 1, "{function}");
            assert!(
                runtime_calls[0].contains("@rt_alloc_atomic(i64 16)"),
                "{function}"
            );
            assert!(
                function.contains("fadd") && function.contains("fmul"),
                "{function}"
            );
        } else {
            assert!(runtime_calls.is_empty(), "{function}");
        }
    }
}

#[test]
fn closed_stderr_preserves_runtime_exit_and_buffered_stdout() {
    use std::{
        os::{fd::OwnedFd, unix::net::UnixStream},
        process::Stdio,
    };
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("pipe.fun");
    let executable = dir.path().join("pipe");
    std::fs::write(
        &file,
        "main = do { _ <- putStr \"before\"; putStr (error \"broken stderr\") }\n",
    )
    .unwrap();
    let built = Command::new(env!("CARGO_BIN_EXE_fun-ch23"))
        .arg("build")
        .arg(file)
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    let (reader, writer) = UnixStream::pair().unwrap();
    drop(reader);
    let output = Command::new(executable)
        .stderr(Stdio::from(OwnedFd::from(writer)))
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
    assert_eq!(output.stdout, b"before");
}

#[test]
fn eight_hundred_equations_build_without_a_larger_host_stack() {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("equations.fun");
    let executable = dir.path().join("equations");
    let mut source = (0..800)
        .map(|n| format!("f {n} = {n}\n"))
        .collect::<String>();
    source.push_str("f x = -1\nmain = print (f 799)\n");
    std::fs::write(&file, source).unwrap();
    let built = Command::new(env!("CARGO_BIN_EXE_fun-ch23"))
        .arg("build")
        .arg(file)
        .arg("--lint")
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    let output = Command::new(executable).output().unwrap();
    assert!(output.status.success());
    assert_eq!(output.stdout, b"799\n");
}
