use ch14_classes::{core, parse, session::Session};
/// Inherited snapshots focus on the program; class-specific tests cover the Prelude.
pub fn dump(source: &str, program: &core::Program) -> String {
    let mut sess = Session::default();
    let file = sess.sources.add("snapshot.fun", source.into());
    let ast = parse::parse(&mut sess, file).unwrap();
    let renamed = ch14_classes::rename::rename(&mut sess, ast).unwrap();
    let names = renamed
        .groups
        .iter()
        .flat_map(|g| &g.binds)
        .filter(|b| b.span.file == file)
        .map(|b| b.id.name.unique)
        .collect::<std::collections::BTreeSet<_>>();
    program
        .groups
        .iter()
        .filter_map(|g| {
            let binds = g
                .binds
                .iter()
                .filter(|b| names.contains(&b.name.unique()))
                .cloned()
                .collect::<Vec<_>>();
            (!binds.is_empty()).then_some(core::BindGroup {
                recursive: g.recursive,
                binds,
            })
        })
        .map(|g| g.to_string())
        .collect::<Vec<_>>()
        .join("\n")
}
