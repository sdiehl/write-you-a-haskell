mod support;
use ch20_runtime::{
    core, elaborate, eval, format, lexer, parse, session::Session,
    typecheck,
};

fn compile(source: &str) -> core::Program {
    let mut sess = Session::default();
    let file = sess.sources.add("record.fun", source.into());
    let parsed = parse::parse(&mut sess, file).unwrap();
    let typed = typecheck::check(&mut sess, parsed).unwrap();
    let program = elaborate::elaborate(&mut sess, typed).unwrap();
    core::lint(&program).unwrap();
    program
}

#[test]
fn projection_dot_depends_on_token_adjacency() {
    use lexer::Token;
    for (source, projection, section) in [
        ("r.x", true, false),
        ("f.g.h", true, false),
        ("f . g", false, false),
        ("f .g", false, false),
        ("P.x", false, false),
        ("(P 1).x", true, false),
        ("[1].x", true, false),
        ("\"a\".x", true, false),
        ("1.x", true, false),
        ("(.x)", false, true),
        ("( .x)", false, false),
        ("r{- c -}.x", false, false),
    ] {
        let mut sess = Session::default();
        let file = sess.sources.add("dot.fun", source.into());
        let tokens = lexer::lex(&mut sess, file).unwrap();
        assert_eq!(
            tokens.iter().any(|(_, t, _)| *t == Token::Proj),
            projection,
            "{source}"
        );
        assert_eq!(
            tokens.iter().any(|(_, t, _)| *t == Token::ProjSection),
            section,
            "{source}"
        );
    }
}

#[test]
fn comments_cannot_split_formatted_projection_tokens() {
    let source = "data P = P {x :: Int}\nmain = (.x {- after label -}) ((P 3) {- before dot -}).x\n";
    // The input is deliberately ill-typed but both parses must be identical in meaning.
    let mut sess = Session::default();
    let file = sess.sources.add("fmt.fun", source.into());
    let first = format::format(&mut sess, file).unwrap();
    let mut sess = Session::default();
    let file = sess.sources.add("fmt.fun", first.clone());
    let second = format::format(&mut sess, file).unwrap();
    assert_eq!(first, second);
    assert!(first.contains("(.x"));
    assert!(first.contains("{- after label -}"));
    assert!(first.contains("{- before dot -}"));
}

#[test]
fn record_lowering_snapshots() {
    for name in [
        "construct-order",
        "project",
        "update",
        "pattern",
        "shared-signature",
        "constraint-fixpoint",
        "projection-section",
        "nested",
        "polymorphic",
        "shared-update",
    ] {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join(format!("tests/programs/records/{name}.fun"));
        let program = compile(&std::fs::read_to_string(&path).unwrap());
        let dump = support::dump(
            &std::fs::read_to_string(&path).unwrap(),
            &program,
        );
        insta::assert_snapshot!(name, dump);
    }
}

#[test]
fn receiver_precedes_updates_and_fields_follow_source_order() {
    let source = "data P = P {x :: Int,y :: Int}\nmain = (case Nothing of {Just p -> p;}) {x = 1 `div` 0}";
    let program = compile(source);
    assert_eq!(
        eval::run(&program, 10000).unwrap_err(),
        "pattern match failure at record.fun:2:9"
    );
    let source = "data P = P {x :: Int,y :: Int}\nmain = P {y = case Nothing of {Just n -> n;},x = 1 `div` 0}";
    let program = compile(source);
    assert_eq!(
        eval::run(&program, 10000).unwrap_err(),
        "pattern match failure at record.fun:2:15"
    );
}
