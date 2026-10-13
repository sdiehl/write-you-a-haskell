use ch21_llvm::{
    elaborate, eval, layout, lexer::Token, parse, session::Session,
    typecheck,
};

fn run(source: &str) -> String {
    let mut sess = Session::default();
    let file = sess.sources.add("layout.fun", source.into());
    let parsed = parse::parse(&mut sess, file)
        .unwrap_or_else(|_| panic!("{source}\n{:?}", sess.diagnostics));
    let typed = typecheck::check(&mut sess, parsed).unwrap();
    let core = elaborate::elaborate(&mut sess, typed).unwrap();
    eval::run(&core, 10000).unwrap()
}

#[test]
fn implicit_explicit_and_inline_blocks() {
    for source in [
        "main = let x = 40 in x + 2",
        "a = 40;\nmain = a + 2",
        "a = 40 where\nmain = a + 2",
        "main = 42 where",
        "a %% b = a + b\nmain = 40 %% 2",
        "main = let x = let y = 40 in y in x + 2",
        "main = let\n  x = 40\n  y = 2\n in x + y",
        "main = (let x = 40 in x) + 2",
        "main = let x = (let y = 40 in y) in x + 2",
        "{ main = let x = 40 in x + 2; }",
        "main = let { x = 40; y = 2; } in x + y",
        "f x = x + y\n  where\n    y = 2\nmain = f 40",
        "f x = g x\n  where\n    g y = y + 2\nmain = f 40",
        "f x = g x\n  where\n    g y = y + z\n      where\n        z = 2\nmain = f 40",
        "a = let x = 40 in x\nmain = a + 2",
        "main = (let\n  x = 40\n  in x) + 2",
    ] {
        assert_eq!(run(source), "42", "{source}");
    }
}

#[test]
fn future_layout_keywords_are_lexical_only() {
    let mut sess = Session::default();
    let file = sess
        .sources
        .add("future.fun", "main = do\n  case x of\n    y\n  z".into());
    let tokens = layout::layout(&mut sess, file).unwrap();
    assert_eq!(
        tokens.iter().filter(|(_, t, _)| *t == Token::Open).count(),
        3
    );
    assert_eq!(
        tokens.iter().filter(|(_, t, _)| *t == Token::Close).count(),
        3
    );
}

fn formatted(source: &str) -> String {
    let mut sess = Session::default();
    let file = sess.sources.add("format.fun", source.into());
    ch21_llvm::format::format(&mut sess, file)
        .unwrap_or_else(|_| panic!("{source}\n{:?}", sess.diagnostics))
}

#[test]
fn formatter_preserves_comments_and_is_idempotent() {
    for source in [
        "-- leading\nmain = let x = 40 in x + 2 -- trailing\n",
        "main = let\n  -- local\n  x = 40 {- middle {- nested -} -}\n  y = 2\n in x + y\n-- end\n",
        "f x = x + y -- equation\n  where\n    y = 2 -- field\nmain = f 40\n",
        "{main=40+2;}",
        "main = (let x = 40 in x) + 2",
    ] {
        let first = formatted(source);
        assert_eq!(first, formatted(&first), "{source}");
        assert_eq!(run(source), run(&first));
        let comment_texts = |text: &str| {
            use marginalia::Classify;
            let mut sess = Session::default();
            let id = sess.sources.add("comments.fun", text.into());
            ch21_llvm::lexer::raw(&mut sess, id)
                .unwrap()
                .iter()
                .filter_map(|(_, t, _)| {
                    t.trivia().map(|t| t.text.to_string())
                })
                .collect::<Vec<_>>()
        };
        assert_eq!(comment_texts(source), comment_texts(&first));
    }
}

#[test]
fn signatures_are_checked_and_restrict_uses() {
    for source in [
        "id :: a -> a\nid x = x\nmain = id 42",
        "id x = x\nid :: Int -> Int\nmain = id 42",
        "main = let { id :: a -> a; id x = x; n = id True; } in id 42",
        "main = f 42\n  where\n    f :: a -> a\n    f x = x",
        "f :: Int -> Int\nf x = if x == 0 then 42 else f (x - 1)\nmain = f 10",
    ] {
        assert_eq!(run(source), "42");
    }
    for (source, code) in [
        ("f :: a -> a\nf x = 1\nmain = f 42", "E0203"),
        ("id :: Int -> Int\nid x = x\nmain = id True", "E0201"),
        (
            "f x = let { g :: a -> a; g y = x; } in g 1\nmain = f 42",
            "E0203",
        ),
        ("f :: Int\nmain = 42", "E0101"),
        ("main :: Int\nmain :: Int\nmain = 42", "E0102"),
        ("main :: Missing\nmain = 42", "E0103"),
    ] {
        let mut sess = Session::default();
        let file = sess.sources.add("sig.fun", source.into());
        assert!(
            parse::parse(&mut sess, file)
                .and_then(|p| typecheck::check(&mut sess, p))
                .is_err(),
            "{source}"
        );
        assert_eq!(sess.diagnostics[0].code.as_str(), code, "{source}");
    }
}

#[test]
fn operators_fixities_sections_and_haskell_negation() {
    for (source, expected) in [
        (
            "infixr 5 %%\n(%%) x y = x * 10 + y\nmain = 1 %% 2 %% 3",
            "33",
        ),
        (
            "infixl 5 %%\n(%%) x y = x * 10 + y\nmain = 1 %% 2 %% 3",
            "123",
        ),
        ("main = (+) 40 2", "42"),
        ("main = (+ 2) 40", "42"),
        ("main = (40 +) 2", "42"),
        ("main = (- 2) * 21", "-42"),
        ("main = (-) 44 2", "42"),
        ("main = -2 + 3", "1"),
        ("main = -2 * 3 + 4", "-2"),
        ("main = 42 `div` 2 + 21", "42"),
        (
            "infixl 7 `plus`\nplus a b = a + b\nmain = 20 `plus` 1 * 2",
            "42",
        ),
        ("(+) a b = a - b\nmain = 44 + 2", "42"),
        ("main = (40 + 1 +) 1", "42"),
    ] {
        assert_eq!(run(source), expected, "{source}");
        assert_eq!(run(&formatted(source)), expected);
    }
    for source in [
        "main = 1 == 2 == 3",
        "main = 1 + -2",
        "main = (1 + 2 *) 3",
        "infixl 5 %%\ninfixr 5 @@\n(%%) a b = a\n(@@) a b = b\nmain = 1 %% 2 @@ 3",
        "main = 1 +",
        "infixl 12 %%\nmain = 1",
    ] {
        let mut sess = Session::default();
        let file = sess.sources.add("fixity.fun", source.into());
        assert!(
            parse::parse(&mut sess, file)
                .and_then(|p| typecheck::check(&mut sess, p))
                .is_err(),
            "{source}"
        );
        assert_eq!(sess.diagnostics[0].code.as_str(), "E0002", "{source}");
    }
}

fn without_spans(
    mut program: ch21_llvm::ast::Program,
) -> ch21_llvm::ast::Program {
    use ch21_llvm::ast::{Expr, Kind};
    let span =
        ch21_llvm::source::Span::new(ch21_llvm::source::FileId(0), 0, 0);
    fn pattern(p: &mut ch21_llvm::ast::Pat, span: ch21_llvm::source::Span) {
        use ch21_llvm::ast::PatKind;
        p.span = span;
        match &mut p.kind {
            PatKind::Record(_, fields) => {
                for (_, p) in fields {
                    pattern(p, span);
                }
            }
            PatKind::As(_, p) => pattern(p, span),
            PatKind::Con(_, ps)
            | PatKind::List(ps)
            | PatKind::Tuple(ps) => {
                for p in ps {
                    pattern(p, span)
                }
            }
            _ => {}
        }
    }
    fn rows(rs: &mut [ch21_llvm::ast::Row], span: ch21_llvm::source::Span) {
        use ch21_llvm::ast::RowBody;
        for row in rs {
            row.span = span;
            for p in &mut row.patterns {
                pattern(p, span);
            }
            for b in &mut row.locals {
                b.span = span;
                erase(&mut b.rhs, span);
            }
            match &mut row.body {
                RowBody::Plain(e) => erase(e, span),
                RowBody::Guarded(gs) => {
                    for (g, e) in gs {
                        erase(g, span);
                        erase(e, span);
                    }
                }
            }
        }
    }
    fn erase(expr: &mut Expr, span: ch21_llvm::source::Span) {
        expr.span = span;
        match &mut expr.kind {
            Kind::Project(e, _) => erase(e, span),
            Kind::Record(_, fields) => {
                for (_, e) in fields {
                    erase(e, span);
                }
            }
            Kind::Update(e, fields) => {
                erase(e, span);
                for (_, e) in fields {
                    erase(e, span);
                }
            }
            Kind::Equations(rs) => rows(rs, span),
            Kind::Case(e, rs) => {
                erase(e, span);
                rows(rs, span);
            }
            Kind::PatternLam(ps, e) => {
                for p in ps {
                    pattern(p, span);
                }
                erase(e, span);
            }
            Kind::List(es) | Kind::Tuple(es) => {
                for e in es {
                    erase(e, span);
                }
            }
            Kind::Lam(_, a)
            | Kind::Neg(a)
            | Kind::Paren(a)
            | Kind::Annotated(a, _)
            | Kind::RightSection(_, a) => erase(a, span),
            Kind::App(a, b) => {
                erase(a, span);
                erase(b, span);
            }
            Kind::If(c, a, b) => {
                erase(c, span);
                erase(a, span);
                erase(b, span);
            }
            Kind::LetGroup(bindings, body) => {
                for b in bindings {
                    b.span = span;
                    erase(&mut b.rhs, span);
                }
                erase(body, span);
            }
            Kind::Chain(a, pairs, _) => {
                erase(a, span);
                for (_, b) in pairs {
                    erase(b, span);
                }
            }
            _ => {}
        }
    }
    program.span = span;
    for c in &mut program.classes {
        c.span = span;
        for d in &mut c.members {
            match d {
                ch21_llvm::ast::Declaration::Signature(_, _, s) => {
                    *s = span
                }
                ch21_llvm::ast::Declaration::Binding(b) => {
                    b.span = span;
                    erase(&mut b.rhs, span);
                }
                _ => {}
            }
        }
    }
    for i in &mut program.instances {
        i.span = span;
        for b in &mut i.methods {
            b.span = span;
            erase(&mut b.rhs, span);
        }
    }

    for alias in &mut program.aliases {
        alias.span = span;
    }
    for d in &mut program.data {
        d.span = span;
        for c in &mut d.constructors {
            c.span = span;
        }
    }
    for b in &mut program.bindings {
        b.span = span;
        erase(&mut b.rhs, span);
    }
    for f in &mut program.fixities {
        f.span = span;
    }
    program
}

#[test]
fn formatter_round_trips_every_parseable_corpus_ast() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/programs");
    for file in testkit::discover(&root).unwrap() {
        let source = std::fs::read_to_string(&file).unwrap();
        let mut sess = Session::default();
        let id = sess.sources.add("roundtrip.fun", source.clone());
        let Ok(ast) = parse::parse(&mut sess, id) else {
            continue;
        };
        if ch21_llvm::fixity::resolve(&mut sess, ast.clone()).is_err() {
            continue;
        }
        let first = formatted(&source);
        assert_eq!(first, formatted(&first), "{}", file.display());
        let mut sess = Session::default();
        let id = sess.sources.add("roundtrip.fun", first);
        let reparsed = parse::parse(&mut sess, id).unwrap();
        assert_eq!(
            without_spans(ast),
            without_spans(reparsed),
            "{}",
            file.display()
        );
    }
}

#[test]
fn extended_syntax_snapshots() {
    for (name, pass) in [
        ("inline-let", "layout"),
        ("nested-let", "layout"),
        ("where", "layout"),
        ("mixed-blocks", "layout"),
        ("multiple-locals", "layout"),
        ("signature", "types"),
        ("right-fixity", "fixity"),
        ("sections", "fixity"),
        ("negation", "fixity"),
        ("operator-lambda", "fixity"),
    ] {
        let file = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join(format!("tests/programs/layout/{name}.fun"));
        let output =
            std::process::Command::new(env!("CARGO_BIN_EXE_fun-ch21"))
                .args(["dump", "--pass", pass])
                .arg(file)
                .output()
                .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        insta::assert_snapshot!(
            format!("{name}-{pass}"),
            String::from_utf8(output.stdout).unwrap()
        );
    }
}

#[test]
fn empty_blocks_format_idempotently() {
    for source in ["main = let {} in 42", "main = let {\n\n} in 42"] {
        let output = formatted(source);
        assert_eq!(formatted(&output), output);
    }
}

#[test]
fn prelude_fixities_are_inherited_and_locally_overridable() {
    let mut sess = Session::default();
    let file = sess
        .sources
        .add("prelude-fixity.fun", "infixr 0 %%\n".into());
    let inherited = parse::parse(&mut sess, file).unwrap().fixities;
    let file = sess.sources.add("user.fun", "main = 1 %% 2 + 3\n".into());
    let parsed = parse::parse(&mut sess, file).unwrap();
    let resolved = ch21_llvm::fixity::resolve_with_prelude(
        &mut sess, parsed, &inherited,
    )
    .unwrap();
    let inherited_form = resolved.bindings[0].rhs.to_string();
    let file = sess
        .sources
        .add("override.fun", "infixl 8 %%\nmain = 1 %% 2 + 3\n".into());
    let parsed = parse::parse(&mut sess, file).unwrap();
    let resolved = ch21_llvm::fixity::resolve_with_prelude(
        &mut sess, parsed, &inherited,
    )
    .unwrap();
    assert_ne!(inherited_form, resolved.bindings[0].rhs.to_string());
    assert!(inherited_form.contains("+ 2 3"), "{inherited_form}");
}
