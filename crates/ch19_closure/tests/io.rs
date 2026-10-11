mod support;
use ch19_closure::{core, elaborate, parse, session::Session, typecheck};
#[test]
fn io_core_snapshots_lower_actions_to_unit_functions() {
    for name in [
        "hello",
        "pure",
        "order",
        "bind",
        "as-pattern",
        "tuple-pattern",
        "maybe",
        "list",
        "nested",
        "cons-pattern",
    ] {
        let source = std::fs::read_to_string(format!(
            "{}/tests/programs/io/{name}.fun",
            env!("CARGO_MANIFEST_DIR")
        ))
        .unwrap();
        let mut sess = Session::default();
        sess.options.require_io = true;
        let file = sess.sources.add("io.fun", source.clone());
        let ast = parse::parse(&mut sess, file).unwrap();
        let typed = typecheck::check(&mut sess, ast).unwrap();
        assert_eq!(typed.main_description(), "IO ()");
        let program = elaborate::elaborate(&mut sess, typed).unwrap();
        core::lint(&program).unwrap();
        assert_eq!(program.main.ty.to_string(), "() -> ()");
        insta::assert_snapshot!(name, support::dump(&source, &program));
    }
}
#[test]
fn prompt_is_flushed_before_waiting_for_input() {
    use std::{
        io::{Read, Write},
        process::{Command, Stdio},
        sync::mpsc,
        time::Duration,
    };
    let path = format!(
        "{}/tests/programs/io/read.fun",
        env!("CARGO_MANIFEST_DIR")
    );
    let mut child = Command::new(env!("CARGO_BIN_EXE_fun-ch19"))
        .args(["run", &path])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let (tx, rx) = mpsc::channel();
    let reader = std::thread::spawn(move || {
        let mut prefix = [0; 6];
        let result = stdout.read_exact(&mut prefix);
        tx.send(result.map(|()| prefix)).unwrap();
        let mut rest = Vec::new();
        stdout.read_to_end(&mut rest).unwrap();
        rest
    });
    let prefix = match rx.recv_timeout(Duration::from_secs(5)) {
        Ok(Ok(p)) => p,
        other => {
            let _ = child.kill();
            let _ = child.wait();
            panic!("prompt was not flushed: {other:?}")
        }
    };
    assert_eq!(&prefix, b"name? ");
    child
        .stdin
        .take()
        .unwrap()
        .write_all("λ\n".as_bytes())
        .unwrap();
    assert!(child.wait().unwrap().success());
    assert_eq!(reader.join().unwrap(), "λ\n".as_bytes());
}
