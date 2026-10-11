use std::{process::Command, time::Duration};

fn run(args: &[&str], file: &std::path::Path) -> testkit::Outcome {
    testkit::capture(
        Command::new(env!("CARGO_BIN_EXE_fun-ch24"))
            .args(args)
            .arg(file),
        Duration::from_secs(60),
    )
    .expect("compiler completed within the timeout")
}

#[test]
fn long_source_list_reaches_every_backend_and_dump() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("deep.fun");
    let elements = vec!["1"; 5000].join(",");
    std::fs::write(&path, format!("main = print (length [{elements}])\n"))
        .unwrap();
    for engine in ["core", "anf", "flat"] {
        let result = run(&["run", "--lint", "--engine", engine], &path);
        assert_eq!(result.exit, 0, "{engine}: {result:?}");
        assert_eq!(result.stdout, b"5000\n");
    }
    for pass in ["erase", "anf", "flat", "llvm"] {
        let result = run(&["dump", "--lint", "--pass", pass], &path);
        assert_eq!(result.exit, 0, "{pass}: {result:?}");
        assert!(!result.stdout.is_empty());
        // A linear-size input must not produce quadratic debug indentation.
        assert!(result.stdout.len() < 12_000_000, "{pass}");
    }
}

#[test]
fn many_polymorphic_signatures_keep_independent_instantiations() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("signatures.fun");
    let mut source = String::new();
    for n in 0..8000 {
        use std::fmt::Write;
        writeln!(source, "f{n} :: a -> a\nf{n} x = x\n").unwrap();
    }
    source.push_str("main = print (f0 (f7999 42), f0 True)\n");
    std::fs::write(&path, source).unwrap();
    let result = run(&["check", "--lint"], &path);
    assert_eq!(result.exit, 0, "{result:?}");
}

#[test]
fn long_values_release_on_a_small_stack() {
    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(|| {
            use ch24_gc::{core::Lit, data::ConId};
            use std::rc::Rc;
            {
                use ch24_gc::anf_machine::Value;
                let con = ConId {
                    index: 0,
                    name: "Cons".into(),
                };
                let mut value = Value::Lit(Lit::Int(0));
                for _ in 0..50_000 {
                    value = Value::Con(
                        con.clone(),
                        Rc::new(vec![Value::Lit(Lit::Int(1)), value]),
                        None,
                    );
                }
                drop(value);
                let mut value = Value::Lit(Lit::Int(0));
                for _ in 0..50_000 {
                    value = Value::Pap(Rc::new(value), vec![]);
                }
                drop(value);
            }
            {
                use ch24_gc::flat_machine::Value;
                let con = ConId {
                    index: 0,
                    name: "Cons".into(),
                };
                let mut value = Value::Lit(Lit::Int(0));
                for _ in 0..50_000 {
                    value = Value::Con(
                        con.clone(),
                        Rc::new(vec![Value::Lit(Lit::Int(1)), value]),
                        None,
                    );
                }
                drop(value);
                let mut value = Value::Lit(Lit::Int(0));
                for _ in 0..50_000 {
                    value = Value::Pap(Rc::new(value), vec![]);
                }
                drop(value);
            }
        })
        .unwrap()
        .join()
        .unwrap();
}
