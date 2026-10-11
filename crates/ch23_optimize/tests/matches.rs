mod support;
use ch23_optimize::{
    core::{self, Expr, Type},
    elaborate, eval, match_compile, parse,
    session::Session,
    typecheck,
};
fn compile(source: &str) -> (Session, core::Program) {
    let mut sess = Session::default();
    let file = sess.sources.add("match.fun", source.into());
    let ast = parse::parse(&mut sess, file).unwrap();
    let typed = typecheck::check(&mut sess, ast).unwrap();
    let core = elaborate::elaborate(&mut sess, typed).unwrap();
    core::lint(&core).unwrap();
    (sess, core)
}
#[test]
fn decision_trees_preserve_nested_matching_and_guards() {
    let cases = [
        (
            "zip",
            "zip [] ys=[]\nzip xs []=[]\nzip (x:xs) (y:ys)=(x,y):zip xs ys\nmain=zip [1,2] [3,4]",
        ),
        (
            "nested-maybe",
            "f (Just (Just x))=x\nf (Just Nothing)=1\nf Nothing=2\nmain=f (Just (Just 42))",
        ),
        (
            "guard-failure",
            "f (Just x) | x<0=0 | x>10=10\nf (Just x)=x\nf Nothing=99\nmain=f (Just 4)",
        ),
        (
            "guard-locals",
            "f x | y>10=y where {y=x+1}\nf x=x\nmain=f 2",
        ),
        ("literal-default", "f 0=1\nf 2=3\nf x=x\nmain=f 42"),
        (
            "strings",
            "f \"λ\"=1\nf \"hello\"=2\nf s=3\nmain=(f \"λ\",f \"other\")",
        ),
        (
            "as-patterns",
            "f whole@(Just (x:xs)) | x>0=(whole,x)\nf x=(x,0)\nmain=f (Just [42])",
        ),
        (
            "shared-rhs",
            "f (Just x) True=x\nf x b=0\nmain=f (Just 42) False",
        ),
        ("characters", "f 'λ'=1\nf 'a'=2\nf c=3\nmain=f 'λ'"),
        ("double-zero", "f 0.0=1\nf x=2\nmain=f (-0.0)"),
    ];
    for (name, source) in cases {
        let (mut sess, before) = compile(source);
        let expected = eval::run(&before, 1_000_000).unwrap();
        let after = match_compile::compile(&mut sess, before).unwrap();
        core::lint(&after).unwrap();
        assert_eq!(eval::run(&after, 1_000_000).unwrap(), expected);
        insta::assert_snapshot!(name, support::dump(source, &after));
    }
}
#[test]
fn usefulness_finds_missing_and_redundant_clauses() {
    for (source, codes) in [
        ("f True=1\nmain=0", vec!["W0001"]),
        ("f x=1\nf True=2\nmain=0", vec!["W0002"]),
        ("f (Just True)=1\nf Nothing=0\nmain=0", vec!["W0001"]),
        ("f True=1\nf False=0\nmain=0", vec![]),
        ("f x | x>0=1\nmain=0", vec!["W0001"]),
        ("f x | otherwise=1\nmain=f 0", vec![]),
        ("otherwise=False\nf x | otherwise=1\nmain=0", vec!["W0001"]),
    ] {
        let (mut sess, core) = compile(source);
        match_compile::compile(&mut sess, core).unwrap();
        assert_eq!(
            sess.diagnostics
                .iter()
                .map(|d| d.code.as_str())
                .collect::<Vec<_>>(),
            codes
        );
        if codes.contains(&"W0001") {
            assert!(
                sess.diagnostics.last().unwrap().notes[0]
                    .starts_with("missing:")
            );
        }
    }
}
#[test]
fn lint_rejects_non_tail_and_unsaturated_jumps() {
    for non_tail in [false, true] {
        let (mut sess, mut program) = compile("main=1");
        let join = sess.fresh_id("$test");
        let parameter = core::Id {
            name: sess.fresh_id("x"),
            ty: Type::Int,
        };
        let mut jump = Expr::Jump(
            join.clone(),
            if non_tail {
                vec![Expr::Lit(core::Lit::Int(1))]
            } else {
                vec![]
            },
            Type::Int,
        );
        if non_tail {
            jump = Expr::Prim(
                ch23_optimize::ast::Op::Add,
                vec![],
                vec![jump, Expr::Lit(core::Lit::Int(1))],
                ch23_optimize::source::Span::new(
                    ch23_optimize::source::FileId(0),
                    0,
                    1,
                ),
            );
        }
        let rhs = Expr::Join(
            join,
            vec![parameter.clone()],
            Box::new(Expr::Var(parameter)),
            Box::new(jump),
        );
        let main = program
            .groups
            .iter_mut()
            .flat_map(|g| &mut g.binds)
            .find(|b| b.name.name.text == "main")
            .unwrap();
        *main.rhs = rhs;
        let error = core::lint(&program).unwrap_err();
        assert!(
            error.contains(if non_tail {
                "tail position"
            } else {
                "unsaturated"
            }),
            "{error}"
        );
    }
}
#[test]
fn usefulness_agrees_with_exhaustive_boolean_enumeration() {
    use core::Pat;
    let (_, program) = compile("main=1");
    let truth = program
        .constructors
        .iter()
        .find(|c| c.id.name == "True")
        .unwrap()
        .id
        .clone();
    let falsity = program
        .constructors
        .iter()
        .find(|c| c.id.name == "False")
        .unwrap()
        .id
        .clone();
    let choices = [
        Pat::Wild,
        Pat::Con(truth.clone(), vec![]),
        Pat::Con(falsity, vec![]),
    ];
    let rows = (0..3)
        .flat_map(|a| (0..3).map(move |b| (a, b)))
        .map(|(a, b)| vec![choices[a].clone(), choices[b].clone()])
        .collect::<Vec<_>>();
    let matches = |p: &Pat, b: bool| match p {
        Pat::Wild => true,
        Pat::Con(c, _) => (c == &truth) == b,
        _ => unreachable!(),
    };
    for mask in 0..512 {
        let matrix = rows
            .iter()
            .enumerate()
            .filter(|(i, _)| mask & (1 << i) != 0)
            .map(|(_, r)| r.clone())
            .collect::<Vec<_>>();
        for query in &rows {
            let uncovered = [false, true].iter().any(|a| {
                [false, true].iter().any(|b| {
                    matches(&query[0], *a)
                        && matches(&query[1], *b)
                        && !matrix.iter().any(|r| {
                            matches(&r[0], *a) && matches(&r[1], *b)
                        })
                })
            });
            assert_eq!(
                ch23_optimize::usefulness::useful(
                    &matrix,
                    query,
                    &[Type::Bool, Type::Bool],
                    &program.constructors
                )
                .is_some(),
                uncovered,
                "matrix {mask}"
            );
        }
    }
}

#[test]
fn core_calls_use_explicit_continuations() {
    let (_, core) = compile(
        "sum n = if n == 0 then 0 else n + sum (n - 1)\nmain = sum 20000",
    );
    std::thread::scope(|scope| {
        std::thread::Builder::new()
            .stack_size(256 * 1024)
            .spawn_scoped(scope, || {
                assert_eq!(
                    eval::run(&core, 10_000_000).unwrap(),
                    "200010000"
                )
            })
            .unwrap()
            .join()
            .unwrap();
    });
}

#[test]
fn releasing_core_lists_uses_bounded_host_stack() {
    let (_, core) = compile(
        "build n = if n == 0 then [] else n : build (n-1)\ncount xs = case xs of { [] -> 0; x:rest -> 1 + count rest }\nmain = count (build 20000)",
    );
    std::thread::scope(|scope| {
        std::thread::Builder::new()
            .stack_size(256 * 1024)
            .spawn_scoped(scope, || {
                assert_eq!(eval::run(&core, 10_000_000).unwrap(), "20000")
            })
            .unwrap()
            .join()
            .unwrap();
    });
}

#[test]
fn plain_rows_do_not_duplicate_the_remaining_matrix() {
    let mut sizes = Vec::new();
    for count in [64, 128] {
        let mut source = (0..count)
            .map(|n| format!("f {n} = {n}\n"))
            .collect::<String>();
        source.push_str("f x = -1\nmain = f 0\n");
        let (mut sess, before) = compile(&source);
        let after = match_compile::compile(&mut sess, before).unwrap();
        core::lint(&after).unwrap();
        let binding = after
            .groups
            .iter()
            .flat_map(|g| &g.binds)
            .find(|b| b.name.name.text == "f")
            .unwrap();
        let printed = binding.rhs.to_string();
        assert!(!printed.contains("$fail"));
        sizes.push(printed.len());
    }
    assert!(
        sizes[1] < 3 * sizes[0],
        "plain decision trees must grow linearly"
    );
}

#[test]
fn long_join_regions_lint_on_a_small_host_stack() {
    let mut sess = Session::default();
    let main = core::Id {
        name: sess.fresh_id("main"),
        ty: Type::Int,
    };
    let mut body = Expr::Lit(core::Lit::Int(0));
    for _ in 0..4000 {
        body = Expr::Join(
            sess.fresh_id("join"),
            vec![],
            Box::new(Expr::Lit(core::Lit::Int(0))),
            Box::new(body),
        );
    }
    let program = core::Program {
        groups: vec![core::BindGroup {
            recursive: false,
            binds: vec![core::Bind {
                name: main.clone(),
                rhs: Box::new(body),
            }],
        }],
        main,
        data_types: vec![],
        type_kinds: Default::default(),
        constructors: vec![],
        locations: vec![],
        global_types: Default::default(),
    };
    // Borrow the tree so this regression isolates lint from tree destruction.
    std::thread::scope(|scope| {
        std::thread::Builder::new()
            .stack_size(256 * 1024)
            .spawn_scoped(scope, || core::lint(&program))
            .unwrap()
            .join()
            .unwrap()
            .unwrap();
    });
}
