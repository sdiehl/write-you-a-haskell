use ch23_optimize::{elaborate, parse, session::Session, typecheck};
#[test]
fn constructor_metadata_matches_the_fixed_runtime_tags() {
    let mut sess = Session::default();
    let file = sess.sources.add(
        "tags.fun",
        "data Color = Red | Green | Blue\nmain = ()".into(),
    );
    let ast = parse::parse(&mut sess, file).unwrap();
    let typed = typecheck::check(&mut sess, ast).unwrap();
    let core = elaborate::elaborate(&mut sess, typed).unwrap();
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
        ("Red", 0),
        ("Green", 1),
        ("Blue", 2),
    ] {
        assert_eq!(
            core.constructors
                .iter()
                .find(|c| c.id.name == name)
                .unwrap()
                .tag,
            tag,
            "{name}"
        );
    }
}
