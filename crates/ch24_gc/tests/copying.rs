use std::{path::Path, time::Duration};

#[test]
fn corpus_collects_at_every_allocation() {
    testkit::run_copying_corpus(&testkit::Config {
        binary: Path::new(env!("CARGO_BIN_EXE_fun-ch24")),
        programs: &Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/programs"),
        engines: &[testkit::Engine::Native],
        timeout: Duration::from_secs(1800),
    })
    .unwrap();
}

#[test]
fn join_back_edges_retain_captures_but_not_dead_bindings() {
    use ch24_gc::{
        core::Lit,
        flat::{Atom, Flat, Rhs},
        liveness,
        session::Session,
    };
    use std::{collections::BTreeSet, rc::Rc};
    let mut sess = Session::default();
    let held = sess.fresh_id("held");
    let dead = sess.fresh_id("dead");
    let j = sess.fresh_id("loop");
    let p = sess.fresh_id("parameter");
    let t = sess.fresh_id("read-capture");
    let back = Rc::new(Flat::Jump(j.clone(), vec![Atom::Var(p.clone())]));
    let loop_body = Rc::new(Flat::Let(
        t,
        Rhs::Atom(Atom::Var(held.clone())),
        back.clone(),
    ));
    let enter =
        Rc::new(Flat::Jump(j.clone(), vec![Atom::Lit(Lit::Int(0))]));
    let join =
        Rc::new(Flat::Join(j, vec![p.clone()], loop_body, enter.clone()));
    let body = Rc::new(Flat::Let(
        dead.clone(),
        Rhs::Atom(Atom::Lit(Lit::Int(9))),
        join,
    ));
    let live = liveness::analyze(&body);
    assert_eq!(
        live.before[&liveness::key(&enter)],
        BTreeSet::from([held.clone()])
    );
    assert_eq!(
        live.before[&liveness::key(&back)],
        BTreeSet::from([held, p])
    );
    assert!(live.before.values().all(|set| !set.contains(&dead)));
}

#[test]
fn copying_executable_has_no_boehm_dependency() {
    use std::process::Command;
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("main.fun");
    let executable = dir.path().join("main");
    std::fs::write(&source, "main = print [1,2,3]\n").unwrap();
    let built = Command::new(env!("CARGO_BIN_EXE_fun-ch24"))
        .arg("build")
        .arg(&source)
        .arg("--gc=copying")
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
    let dependencies = if cfg!(target_os = "macos") {
        Command::new("otool")
            .arg("-L")
            .arg(&executable)
            .output()
            .unwrap()
    } else {
        Command::new("ldd").arg(&executable).output().unwrap()
    };
    assert!(dependencies.status.success());
    assert!(
        !String::from_utf8_lossy(&dependencies.stdout).contains("libgc.")
    );
    let output = Command::new(executable)
        .env("FUN_GC_STRESS", "1")
        .output()
        .unwrap();
    assert!(output.status.success());
    assert_eq!(output.stdout, b"[1,2,3]\n");
}
