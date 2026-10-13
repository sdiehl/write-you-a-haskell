use ch18_anf::{
    anf, anf_lint, anf_machine, anf_simplify, core, elaborate, erase, eval,
    match_compile, parse, session::Session, typecheck,
};

fn compile(source: &str) -> (Session, core::Program) {
    let mut sess = Session::default();
    let file = sess.sources.add("anf.fun", source.into());
    let p = parse::parse(&mut sess, file).unwrap();
    let p = typecheck::check(&mut sess, p)
        .unwrap_or_else(|_| panic!("{:?}", sess.diagnostics));
    let p = elaborate::elaborate(&mut sess, p).unwrap();
    core::lint(&p).unwrap();
    let p = match_compile::compile(&mut sess, p).unwrap();
    core::lint(&p).unwrap();
    (sess, p)
}

#[test]
fn anf_matches_core_before_and_after_simplification() {
    for (name, source) in [
        ("arithmetic", "main = (1+2)*3"),
        ("let", "main = let { x=21; } in x+x"),
        ("self-application", "main=let { id x=x; } in id id 42"),
        (
            "non-tail-case",
            "main = 1 + (case Just 41 of { Nothing -> 0; Just n -> n })",
        ),
        ("guard-join", "f x | x<0=0 | x>10=10\nf x=x\nmain=f 3"),
        ("capture", "f x = \\y -> x+y\nmain=f 20 22"),
        ("partial", "main=map (+1) [1,2,3]"),
        ("over-apply", "main=(\\f -> f) (\\x y -> x-y) 10 3"),
        (
            "letrec",
            "main=let { f n=if n==0 then 7 else g (n-1); g n=f n; } in f 20",
        ),
        (
            "strict-let",
            "main=let { x=error \"unused\" :: Int; } in 42",
        ),
        ("beta", "main=(\\x -> x+1) 41"),
        (
            "beta-nontail",
            "main=1 + ((\\x -> if x>0 then x+1 else 0) 40)",
        ),
    ] {
        let (mut sess, core) = compile(source);
        let expected = eval::run(&core, 1_000_000);
        let mut anf = anf::convert(&mut sess, erase::erase(core));
        for optimize in [false, true] {
            if optimize {
                anf_simplify::simplify(&mut sess, &mut anf);
            }
            anf_lint::lint(&anf).unwrap();
            let actual = anf_machine::run(&anf, 1_000_000, false)
                .map(|(v, _)| v.to_string())
                .map_err(|e| e.to_string());
            assert_eq!(actual, expected, "{name} optimized={optimize}");
        }
        let text = anf
            .globals
            .iter()
            .filter(|(v, _)| ["main", "f", "g"].contains(&v.text.as_str()))
            .map(|(v, b)| format!("{} = {b}", v.text))
            .collect::<Vec<_>>()
            .join("\n");
        insta::assert_snapshot!(name, text);
    }
}

#[test]
fn one_million_tail_calls_use_bounded_continuations() {
    let (mut sess, core) = compile(
        "sumTo n = go 0 n where { go acc n = if n == 0 then acc else go (acc+n) (n-1); }\nmain = sumTo 1000000",
    );
    let mut anf = anf::convert(&mut sess, erase::erase(core));
    anf_simplify::simplify(&mut sess, &mut anf);
    anf_lint::lint(&anf).unwrap();
    let (value, stats) = anf_machine::run(&anf, usize::MAX, false).unwrap();
    assert_eq!(value.to_string(), "500000500000");
    assert!(stats.max_continuations < 32, "{stats:?}");
}

#[test]
fn one_thousand_generated_programs_agree() {
    testkit::generate::check(
        std::path::Path::new(env!("CARGO_BIN_EXE_fun-ch18")),
        &[testkit::Engine::Core, testkit::Engine::Anf],
        0x46554e,
        1000,
    )
    .unwrap();
}
