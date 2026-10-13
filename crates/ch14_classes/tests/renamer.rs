use ch14_classes::{
    core, elaborate, eval, parse, rename, session::Session, typecheck,
};

fn execute(
    source: &str,
) -> Result<String, Vec<ch14_classes::diagnostics::Diagnostic>> {
    let mut sess = Session::default();
    let file = sess.sources.add("test.fun", source.into());
    let result = parse::parse(&mut sess, file)
        .and_then(|p| typecheck::check(&mut sess, p))
        .and_then(|t| elaborate::elaborate(&mut sess, t));
    match result {
        Ok(program) => {
            core::lint(&program).unwrap();
            Ok(eval::run(&program, 100000).unwrap())
        }
        Err(_) => Err(sess.diagnostics),
    }
}

#[test]
fn forward_references_and_mutual_recursion() {
    for source in [
        "main = f 42\nf x = id x\nid x = x",
        "even n = if n == 0 then True else odd (n-1)\nodd n = if n == 0 then False else even (n-1)\nmain = if even 100 then 42 else 0",
        "main = let { f x = g x; g x = x; } in f 42",
        "main = let { even n = if n == 0 then True else odd (n-1); odd n = if n == 0 then False else even (n-1); } in if even 100 then 42 else 0",
        "main = result\nresult = id 42\nflag = id True\nid x = x",
        "x = 100\nmain = let { x = 42; } in x",
        "f x = let { g y = if y == 0 then x else h (y-1); h y = g y; } in g 10\nmain = f 42",
        "main = f 42\nf x = g x\n  where\n    g y = h y\n    h z = z",
    ] {
        assert_eq!(execute(source).unwrap(), "42", "{source}");
    }
}

#[test]
fn scope_errors_collect_and_suggest() {
    let errors = execute("hello = 1\nmain = helo + absent").unwrap_err();
    assert_eq!(errors.len(), 2);
    assert!(errors[0].notes.iter().any(|s| s.contains("hello")));
    for (source, code) in [
        ("main = let { x = 1; x = 2; } in x", "E0102"),
        ("a = b\nb = a\nmain = 0", "E0120"),
        ("infixl 5 %%\nmain = 42", "E0130"),
        ("main = Missing", "E0103"),
        ("main = prim_error", "E0111"),
        (
            "f :: Int -> Int\nf x = g x\ng y = f y\nmain = g True",
            "E0201",
        ),
    ] {
        assert_eq!(
            execute(source).unwrap_err()[0].code.as_str(),
            code,
            "{source}"
        );
    }
    assert_eq!(rename::distance("kitten", "sitting"), 3);
    assert_eq!(rename::distance("foo", "fo"), 1);
}

#[test]
fn tarjan_agrees_with_reachability_and_orders_dependencies() {
    let mut seed = 17_u64;
    for n in 1..20 {
        for _ in 0..30 {
            let mut edges = vec![vec![]; n];
            for row in &mut edges {
                for j in 0..n {
                    seed = seed
                        .wrapping_mul(6364136223846793005)
                        .wrapping_add(1);
                    if seed >> 61 == 0 {
                        row.push(j);
                    }
                }
            }
            let groups = ch14_classes::scc::components(&edges);
            let mut owner = vec![usize::MAX; n];
            for (g, vertices) in groups.iter().enumerate() {
                for &v in vertices {
                    assert_eq!(owner[v], usize::MAX);
                    owner[v] = g;
                }
            }
            let reachable: Vec<_> = (0..n)
                .map(|start| {
                    let mut seen = vec![false; n];
                    let mut pending = vec![start];
                    while let Some(v) = pending.pop() {
                        if !seen[v] {
                            seen[v] = true;
                            pending.extend(&edges[v]);
                        }
                    }
                    seen
                })
                .collect();
            for v in 0..n {
                for w in 0..n {
                    assert_eq!(
                        owner[v] == owner[w],
                        reachable[v][w] && reachable[w][v]
                    );
                }
                for &w in &edges[v] {
                    assert!(owner[w] <= owner[v]);
                }
            }
        }
    }
}

#[test]
fn renaming_makes_shadowed_binders_distinct() {
    let mut sess = Session::default();
    let file = sess.sources.add(
        "names.fun",
        "x = 1\nf x = let { x = 2; } in x\nmain = f x".into(),
    );
    let parsed = parse::parse(&mut sess, file).unwrap();
    let program = rename::rename(&mut sess, parsed).unwrap();
    let dump = program
        .groups
        .iter()
        .filter_map(|g| {
            let binds = g
                .binds
                .iter()
                .filter(|b| b.span.file == file)
                .cloned()
                .collect::<Vec<_>>();
            (!binds.is_empty()).then_some(
                ch14_classes::renamed::BindGroup {
                    recursive: g.recursive,
                    binds,
                },
            )
        })
        .map(|g| g.to_string())
        .collect::<Vec<_>>()
        .join("\n");
    insta::assert_snapshot!("shadowed-identities", dump);
}
