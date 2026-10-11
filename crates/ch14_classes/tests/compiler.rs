mod support;
use ch14_classes::{
    ast, core,
    diagnostics::{Diagnostic, ErrorCode},
    elaborate, eval, lexer, parse,
    session::Session,
    source::Span,
    typecheck,
};
fn compile(source: &str) -> Result<(String, core::Program), String> {
    let mut sess = Session::default();
    let file = sess.sources.add("test.fun", source.into());
    let result = parse::parse(&mut sess, file)
        .and_then(|p| typecheck::check(&mut sess, p));
    match result {
        Ok(typed) => {
            let ty = typed.main_type.to_string();
            let core = elaborate::elaborate(&mut sess, typed).unwrap();
            core::lint(&core).unwrap();
            Ok((ty, core))
        }
        Err(_) => Err(sess.diagnostics[0].code.as_str().into()),
    }
}
#[test]
fn source_maps_use_file_identity_byte_offsets_and_scalar_columns() {
    let mut sess = Session::default();
    let a = sess.sources.add("a.fun", "a\nλ = 1\n".into());
    let b = sess.sources.add("b.fun", "é".into());
    assert_ne!(a, b);
    assert_eq!(sess.sources.line_col(Span::new(a, 4, 5)), (2, 2));
    let diag = Diagnostic::error(
        ErrorCode::Mismatch,
        "expected Int, found Bool",
        Span::new(a, 7, 8),
    );
    let mut output = vec![];
    diag.render(&sess.sources, false, &mut output).unwrap();
    assert!(
        String::from_utf8(output)
            .unwrap()
            .starts_with("E0201\na.fun:2:5\n")
    );
    let symbol = sess.interner.intern("x");
    assert_eq!(symbol, sess.interner.intern("x"));
    assert_eq!(sess.interner.resolve(symbol), "x");
    assert_ne!(sess.supply.fresh(symbol), sess.supply.fresh(symbol));
}
#[test]
fn literals_comments_and_operator_maximal_munch() {
    let source = r#"{ {- outer {- nested -} -} main = "λ\n" ++ "\u{1f642}"; } -- end"#;
    let (ty, program) = compile(source).unwrap();
    assert_eq!(ty, "String");
    assert_eq!(eval::run(&program, 1000).unwrap(), "\"λ\\n🙂\"");
    assert_eq!(compile("{ main = '\\u{d800}'; }").unwrap_err(), "E0001");
    assert_eq!(
        compile("{ main = 1; } {- unfinished").unwrap_err(),
        "E0001"
    );
    let mut sess = Session::default();
    let file = sess.sources.add("operator.fun", "--> -- comment\n".into());
    let tokens = lexer::lex(&mut sess, file).unwrap();
    assert!(matches!(&tokens[0].1,lexer::Token::Operator(s) if s=="-->"));
    assert_eq!(tokens.len(), 1);
    assert_eq!(ast::quote("'\n\0", '\''), "'\\'\\n\\0'");
}
#[test]
fn ordered_recursion_scoping_and_initialization() {
    for (source, expected) in [
        ("{ main = main; }", "E0120"),
        ("{ main = 1; main = 2; }", "E0102"),
        ("{ f x x = x; main = 1; }", "E0102"),
        ("{ main = prim_intAdd; }", "E0111"),
        ("{ main = Missing; }", "E0103"),
    ] {
        assert_eq!(compile(source).unwrap_err(), expected);
    }
    let (_, program) =
        compile("{ f n = if n == 0 then 0 else f (n - 1); main = f 100; }")
            .unwrap();
    assert_eq!(eval::run(&program, 10000).unwrap(), "0");
    let (_, program) = compile("{ unused = div 1 0; main = 42; }").unwrap();
    assert_eq!(eval::run(&program, 10000).unwrap_err(), "divide by zero");
}
#[test]
fn floor_division_and_wrapping() {
    for (expression, expected) in [
        ("div (-7) 3", "-3"),
        ("mod (-7) 3", "2"),
        ("div 7 (-3)", "-3"),
        ("mod 7 (-3)", "-2"),
        ("div (-7) (-3)", "2"),
        ("mod (-7) (-3)", "-1"),
        ("4611686018427387903 + 1", "-4611686018427387904"),
    ] {
        let (_, program) =
            compile(&format!("{{ main = {expression}; }}")).unwrap();
        assert_eq!(eval::run(&program, 1000).unwrap(), expected);
    }
}
#[test]
fn core_lint_rejects_an_unbound_compiler_generated_variable() {
    let (_, mut program) = compile("{ main = 1; }").unwrap();
    let missing = Session::default().fresh_id("missing");
    *program
        .groups
        .iter_mut()
        .flat_map(|g| &mut g.binds)
        .find(|b| b.name.name.text == "main")
        .unwrap()
        .rhs = core::Expr::Var(core::Id {
        name: missing,
        ty: core::Type::Int,
    });
    assert!(core::lint(&program).is_err());
}
#[test]
fn golden_corpus() {
    testkit::run_corpus(&testkit::Config {
        binary: std::path::Path::new(env!("CARGO_BIN_EXE_fun-ch14")),
        programs: &std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/programs"),
        engines: &[testkit::Engine::Core],
        timeout: std::time::Duration::from_secs(10),
    })
    .unwrap();
}
#[test]
fn core_snapshots() {
    for (name, source) in [
        ("literal", "{main=42;}"),
        ("arithmetic", "{main=1+2*3;}"),
        ("identity", "{id x=x; main=id 42;}"),
        ("polymorphism", "{id x=x; a=id 1; main=id True;}"),
        ("recursion", "{f n=if n==0 then 0 else f (n-1); main=f 3;}"),
        ("closure", "{x=1; f y=x+y; main=f 2;}"),
        ("string", "{main=\"a\"++\"b\";}"),
        ("char", "{main='λ';}"),
        ("local", "{main=let {id x=x;} in id 3;}"),
        ("shadow", "{x=1; main=let {x=2;} in x;}"),
    ] {
        let (ty, program) = compile(source).unwrap();
        let mut dump = format!("main : {ty}\n");
        dump.push_str(&support::dump(source, &program));
        insta::assert_snapshot!(name, dump);
    }
}

#[test]
fn integer_literals_wrap_63_bits() {
    let mut sess = Session::default();
    let file = sess.sources.add(
        "integer.fun",
        "4611686018427387904 0x4000000000000000 9223372036854775807".into(),
    );
    let tokens = lexer::raw(&mut sess, file).unwrap();
    let values = tokens
        .into_iter()
        .filter_map(|(_, t, _)| match t {
            lexer::Token::Int(n) => Some(n),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        values,
        vec![-4611686018427387904, -4611686018427387904, -1]
    );
}
