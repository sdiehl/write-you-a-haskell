mod support;

use ch18_anf::{core, elaborate, eval, parse, session::Session, typecheck};

fn compile(source: &str) -> core::Program {
    let mut sess = Session::default();
    let file = sess.sources.add("data.fun", source.into());
    let parsed = parse::parse(&mut sess, file).unwrap();
    let typed = typecheck::check(&mut sess, parsed).unwrap();
    let program = elaborate::elaborate(&mut sess, typed).unwrap();
    core::lint(&program).unwrap();
    program
}

#[test]
fn datatype_core_snapshots() {
    for name in [
        "list-map",
        "list-fold",
        "tree",
        "pattern-as",
        "guard-fallthrough",
        "guard-where",
        "case-literals",
        "constructor-signature",
        "pattern-binding-poly",
        "boolean-short-circuit",
    ] {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join(format!("tests/programs/data/{name}.fun"));
        let core = compile(&std::fs::read_to_string(&path).unwrap());
        let dump =
            support::dump(&std::fs::read_to_string(&path).unwrap(), &core);
        insta::assert_snapshot!(name, dump);
    }
}

#[test]
fn core_lint_checks_constructor_instantiation_and_pattern_binders() {
    let program = compile("main = Just 1");
    let just = program
        .constructors
        .iter()
        .find(|c| c.id.name == "Just")
        .unwrap()
        .id
        .clone();
    for rhs in [
        core::Expr::Con(
            just.clone(),
            vec![],
            vec![core::Expr::Lit(core::Lit::Int(1))],
        ),
        core::Expr::Con(just.clone(), vec![core::Type::Int], vec![]),
        core::Expr::Con(
            just,
            vec![core::Type::Bool],
            vec![core::Expr::Lit(core::Lit::Int(1))],
        ),
    ] {
        let mut malformed = program.clone();
        *malformed
            .groups
            .iter_mut()
            .flat_map(|g| &mut g.binds)
            .find(|b| b.name.name.text == "main")
            .unwrap()
            .rhs = rhs;
        assert!(core::lint(&malformed).is_err());
    }
    let mut program = compile("main = case Just 1 of { Just x -> x; }");
    let core::Expr::Let(_, body) = &mut *program
        .groups
        .iter_mut()
        .flat_map(|g| &mut g.binds)
        .find(|b| b.name.name.text == "main")
        .unwrap()
        .rhs
    else {
        panic!("scrutinee let")
    };
    let core::Expr::Match(m) = &mut **body else {
        panic!("match")
    };
    let core::Pat::Con(_, ps) = &mut m.rows[0].pats[0] else {
        panic!("pattern")
    };
    let core::Pat::Var(id) = &mut ps[0] else {
        panic!("binder")
    };
    id.ty = core::Type::Bool;
    assert!(core::lint(&program).unwrap_err().contains("type mismatch"));
}

#[test]
fn strict_order_and_guard_fallthrough_are_observable() {
    let program =
        compile("main = (case Nothing of { Just x -> x; }, 1 `div` 0)");
    assert_eq!(
        eval::run(&program, 10000).unwrap_err(),
        "pattern match failure at data.fun:1:9"
    );
    let program =
        compile("main = (1 `div` 0, case Nothing of { Just x -> x; })");
    assert_eq!(eval::run(&program, 10000).unwrap_err(), "divide by zero");
    let program = compile(
        "f Nothing | True = 0 where { bad = 1 `div` 0; }\nf (Just x) | False = 0\nf (Just x) = x\nmain = f (Just 42)",
    );
    assert_eq!(eval::run(&program, 10000).unwrap(), "42");
}

#[test]
fn source_prelude_assigns_fixed_constructor_tags() {
    let program = compile("main = ()");
    for (name, tag) in [
        ("False", 0),
        ("True", 1),
        ("[]", 0),
        (":", 1),
        ("Nothing", 0),
        ("Just", 1),
        ("()", 0),
        ("LT", 0),
        ("EQ", 1),
        ("GT", 2),
    ] {
        assert_eq!(
            program
                .constructors
                .iter()
                .find(|c| c.id.name == name)
                .unwrap()
                .tag,
            tag
        );
    }
}
