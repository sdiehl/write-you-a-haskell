//! Scope checking allocates unique names before computing dependency components.
use crate::{
    ast::{self, Op},
    diagnostics::{Diagnostic, ErrorCode},
    names::Id,
    renamed::{self, BindGroup, Binding, Expr, Kind, Program},
    session::{Session, Stop},
    source::Span,
};
use std::collections::{BTreeMap, BTreeSet};
struct Renamer<'a> {
    prelude_file: Option<crate::source::FileId>,
    data: crate::data::DataEnv,
    sess: &'a mut Session,
    scopes: Vec<BTreeMap<String, Id>>,
}
impl Renamer<'_> {
    fn fresh(&mut self, name: &str, span: Span) -> Id {
        if name.starts_with("prim_") {
            self.sess.emit(Diagnostic::error(
                ErrorCode::PrivatePrimitive,
                "primitive names are private to the Prelude",
                span,
            ));
        }
        self.sess.fresh_id(name)
    }
    fn lookup(&mut self, name: &str, span: Span) -> Id {
        if name.starts_with("prim_") && Some(span.file) != self.prelude_file
        {
            self.sess.emit(Diagnostic::error(
                ErrorCode::PrivatePrimitive,
                "primitive names are private to the Prelude",
                span,
            ));
            return self.sess.fresh_id(name);
        }
        if let Some(id) = self.scopes.iter().rev().find_map(|s| s.get(name))
        {
            return id.clone();
        }
        let code = if name.starts_with("prim_") {
            ErrorCode::PrivatePrimitive
        } else if name.starts_with(char::is_uppercase) {
            ErrorCode::UnboundConstructor
        } else {
            ErrorCode::Unbound
        };
        let mut error = Diagnostic::error(
            code,
            format!("unbound variable {name}"),
            span,
        );
        if code == ErrorCode::Unbound {
            let candidate = self
                .scopes
                .iter()
                .flat_map(|s| s.keys())
                .map(|n| (distance(name, n), n))
                .filter(|(d, _)| *d <= 2)
                .min();
            if let Some((_, candidate)) = candidate {
                error.notes.push(format!("did you mean `{candidate}`?"));
            }
        }
        self.sess.emit(error);
        self.sess.fresh_id(name)
    }
    fn bindings(&mut self, bindings: Vec<ast::Binding>) -> Vec<BindGroup> {
        let mut scope = BTreeMap::new();
        let mut identities = Vec::new();
        for binding in &bindings {
            let id = self.fresh(&binding.name, binding.span);
            if scope.insert(binding.name.clone(), id.clone()).is_some() {
                self.sess.emit(Diagnostic::error(
                    ErrorCode::Duplicate,
                    format!("duplicate definition {}", binding.name),
                    binding.span,
                ));
            }
            identities.push(id);
        }
        self.scopes.push(scope);
        let bindings: Vec<_> = bindings
            .into_iter()
            .zip(identities)
            .map(|(b, id)| {
                let (rhs, annotation) = match b.rhs.kind {
                    ast::Kind::Annotated(inner, scheme) => {
                        (*inner, Some(scheme))
                    }
                    _ => (b.rhs, None),
                };
                let hidden = b
                    .nonrecursive
                    .iter()
                    .filter_map(|name| {
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .remove(name)
                            .map(|id| (name.clone(), id))
                    })
                    .collect::<Vec<_>>();
                let rhs = self.expr(rhs);
                self.scopes.last_mut().unwrap().extend(hidden);
                Binding {
                    id,
                    rhs,
                    annotation,
                    span: b.span,
                }
            })
            .collect();
        let indices: BTreeMap<_, _> = bindings
            .iter()
            .enumerate()
            .map(|(i, b)| (b.id.clone(), i))
            .collect();
        let edges: Vec<_> = bindings
            .iter()
            .map(|b| {
                let mut names = BTreeSet::new();
                renamed::references(&b.rhs, &mut names);
                let mut edges = names
                    .into_iter()
                    .filter_map(|n| indices.get(&n).copied())
                    .collect::<Vec<_>>();
                edges.sort_unstable();
                edges
            })
            .collect();
        crate::scc::components(&edges).into_iter().map(|component| {
            let recursive=component.len()>1 || edges[component[0]].contains(&component[0]);
            let binds=component.into_iter().map(|i|bindings[i].clone()).collect::<Vec<_>>();
            if recursive {for b in &binds {if !is_function(&b.rhs) {self.sess.emit(Diagnostic::error(ErrorCode::RecursiveValue,"every recursive binding must be a function",b.rhs.span));}}}
            BindGroup {recursive,binds}
        }).collect()
    }
    fn constructor(
        &mut self,
        name: &str,
        span: Span,
    ) -> crate::data::ConId {
        if let Some(con) = self.data.constructor(name) {
            return con.id.clone();
        }
        self.sess.emit(Diagnostic::error(
            ErrorCode::UnboundConstructor,
            format!("unknown constructor {name}"),
            span,
        ));
        crate::data::ConId {
            index: 0,
            name: name.into(),
        }
    }
    fn pattern_name(&mut self, name: String, span: Span) -> Id {
        if name == "_" {
            self.sess.emit(Diagnostic::error(
                ErrorCode::Parse,
                "a wildcard cannot name an as-pattern",
                span,
            ));
        }
        let id = self.fresh(&name, span);
        if self
            .scopes
            .last_mut()
            .unwrap()
            .insert(name.clone(), id.clone())
            .is_some()
        {
            self.sess.emit(Diagnostic::error(
                ErrorCode::Duplicate,
                format!("duplicate pattern variable {name}"),
                span,
            ));
        }
        id
    }
    fn pattern(&mut self, pattern: ast::Pat) -> renamed::Pat {
        use ast::PatKind as A;
        use renamed::PatKind as R;
        let span = pattern.span;
        let kind = match pattern.kind {
            A::Record(n, fields) => {
                let id = self.constructor(&n, span);
                if self.data.constructor(&n).is_none() {
                    return renamed::Pat {
                        kind: R::Wild,
                        span,
                    };
                }
                let names = fields
                    .iter()
                    .map(|(n, _)| n.clone())
                    .collect::<Vec<_>>();
                match self.data.fields(&id, &names, false, span) {
                    Ok(indices) => {
                        let mut pats = vec![
                            renamed::Pat {
                                kind: R::Wild,
                                span
                            };
                            self.data.con(&id).fields.len()
                        ];
                        for ((_, p), i) in fields.into_iter().zip(indices) {
                            pats[i] = self.pattern(p);
                        }
                        R::Con(id, pats)
                    }
                    Err(error) => {
                        self.sess.emit(error);
                        R::Wild
                    }
                }
            }
            A::Var(n) => R::Var(self.pattern_name(n, span)),
            A::Wild => R::Wild,
            A::Con(n, ps) => R::Con(
                self.constructor(&n, span),
                ps.into_iter().map(|p| self.pattern(p)).collect(),
            ),
            A::As(n, p) => R::As(
                self.pattern_name(n, span),
                Box::new(self.pattern(*p)),
            ),
            A::Double(n) => R::Double(n),
            A::Int(n) => R::Int(n),
            A::Char(c) => R::Char(c),
            A::String(s) => R::String(s),
            A::List(ps) => {
                R::List(ps.into_iter().map(|p| self.pattern(p)).collect())
            }
            A::Tuple(ps) => {
                R::Tuple(ps.into_iter().map(|p| self.pattern(p)).collect())
            }
        };
        renamed::Pat { kind, span }
    }
    fn row(&mut self, row: ast::Row) -> renamed::Row {
        self.scopes.push(BTreeMap::new());
        let patterns =
            row.patterns.into_iter().map(|p| self.pattern(p)).collect();
        let locals = self.bindings(row.locals);
        let body = match row.body {
            ast::RowBody::Plain(e) => {
                renamed::RowBody::Plain(Box::new(self.expr(*e)))
            }
            ast::RowBody::Guarded(gs) => renamed::RowBody::Guarded(
                gs.into_iter()
                    .map(|(g, e)| (self.expr(g), self.expr(e)))
                    .collect(),
            ),
        };
        self.scopes.pop();
        self.scopes.pop();
        renamed::Row {
            patterns,
            body,
            locals,
            span: row.span,
        }
    }
    fn expr(&mut self, expr: ast::Expr) -> Expr {
        // Fixity resolution can produce very long application spines.
        let mut work = vec![(Some(expr), None)];
        let mut values: Vec<Expr> = Vec::new();
        while let Some((node, span)) = work.pop() {
            match node {
                Some(ast::Expr {
                    kind: ast::Kind::App(f, a),
                    span,
                }) => {
                    work.push((None, Some(span)));
                    work.push((Some(*a), None));
                    work.push((Some(*f), None));
                }
                Some(node) => values.push(self.expr_leaf(node)),
                None => {
                    let a = values.pop().expect("application argument");
                    let f = values.pop().expect("application function");
                    values.push(Expr {
                        node: self.sess.fresh_node(),
                        span: span.expect("application span"),
                        kind: Kind::App(Box::new(f), Box::new(a)),
                    });
                }
            }
        }
        values.pop().expect("renamed root")
    }
    fn expr_leaf(&mut self, expr: ast::Expr) -> Expr {
        let span = expr.span;
        let kind = match expr.kind {
            ast::Kind::DoFail => {
                let (line, col) = self.sess.sources.line_col(span);
                let name = self.sess.sources.get(span.file).name.clone();
                let message = format!(
                    "pattern match failure in do at {name}:{line}:{col}"
                );
                let error = ast::Expr {
                    kind: ast::Kind::Var("error".into()),
                    span,
                };
                let text = ast::Expr {
                    kind: ast::Kind::String(message),
                    span,
                };
                return self.expr(ast::Expr {
                    kind: ast::Kind::App(Box::new(error), Box::new(text)),
                    span,
                });
            }

            ast::Kind::Record(n, fields) => Kind::Record(
                self.constructor(&n, span),
                fields
                    .into_iter()
                    .map(|(n, e)| (n, self.expr(e)))
                    .collect(),
            ),
            ast::Kind::Project(e, n) => {
                Kind::Project(Box::new(self.expr(*e)), n)
            }
            ast::Kind::Update(e, fields) => Kind::Update(
                Box::new(self.expr(*e)),
                fields
                    .into_iter()
                    .map(|(n, e)| (n, self.expr(e)))
                    .collect(),
            ),
            ast::Kind::Annotated(e, t) => {
                Kind::Annotated(Box::new(self.expr(*e)), t)
            }
            ast::Kind::Double(n) => Kind::Double(n),
            ast::Kind::Int(n) => Kind::Int(n),
            ast::Kind::Char(c) => Kind::Char(c),
            ast::Kind::String(s) => Kind::String(s),
            ast::Kind::Var(n)
                if n.starts_with(char::is_uppercase)
                    || matches!(
                        n.as_str(),
                        ":" | "[]" | "()" | "(,)" | "(,,)"
                    ) =>
            {
                Kind::Con(self.constructor(&n, span))
            }
            ast::Kind::Var(n) => Kind::Var(self.lookup(&n, span)),
            ast::Kind::PatternLam(patterns, body) => {
                self.scopes.push(BTreeMap::new());
                let patterns =
                    patterns.into_iter().map(|p| self.pattern(p)).collect();
                let body = self.expr(*body);
                self.scopes.pop();
                Kind::PatternLam(patterns, Box::new(body))
            }
            ast::Kind::Equations(rows) => Kind::Equations(
                rows.into_iter().map(|r| self.row(r)).collect(),
            ),
            ast::Kind::Case(e, rows) => Kind::Case(
                Box::new(self.expr(*e)),
                rows.into_iter().map(|r| self.row(r)).collect(),
            ),
            ast::Kind::List(es) => {
                Kind::List(es.into_iter().map(|e| self.expr(e)).collect())
            }
            ast::Kind::Tuple(es) => {
                Kind::Tuple(es.into_iter().map(|e| self.expr(e)).collect())
            }
            ast::Kind::Lam(n, body) => {
                let id = self.fresh(&n, span);
                self.scopes.push(BTreeMap::from([(n, id.clone())]));
                let body = self.expr(*body);
                self.scopes.pop();
                Kind::Lam(id, Box::new(body))
            }
            ast::Kind::App(a, b) => {
                Kind::App(Box::new(self.expr(*a)), Box::new(self.expr(*b)))
            }
            ast::Kind::If(c, a, b) => Kind::If(
                Box::new(self.expr(*c)),
                Box::new(self.expr(*a)),
                Box::new(self.expr(*b)),
            ),
            ast::Kind::Neg(a) => {
                if let Some(id) = self
                    .scopes
                    .iter()
                    .find_map(|s| s.get("negate"))
                    .cloned()
                {
                    let node = self.sess.fresh_node();
                    Kind::App(
                        Box::new(Expr {
                            node,
                            kind: Kind::Var(id),
                            span,
                        }),
                        Box::new(self.expr(*a)),
                    )
                } else {
                    Kind::Neg(Box::new(self.expr(*a)))
                }
            }
            ast::Kind::LetGroup(bindings, body) => {
                let groups = self.bindings(bindings);
                let body = self.expr(*body);
                self.scopes.pop();
                Kind::Let(groups, Box::new(body))
            }
            _ => unreachable!(
                "fixity resolution and declaration collection precede renaming"
            ),
        };
        let node = self.sess.fresh_node();
        Expr { node, kind, span }
    }
}
fn is_function(expr: &Expr) -> bool {
    match &expr.kind {
        Kind::Lam(..) | Kind::PatternLam(..) => true,
        Kind::Equations(rows) => {
            !rows[0].patterns.is_empty()
                || rows.len() == 1
                    && rows[0].locals.is_empty()
                    && matches!(&rows[0].body,renamed::RowBody::Plain(e) if is_function(e))
        }
        _ => false,
    }
}
/// Resolve lexical scopes, validate definitions, and order every binding block.
// ANCHOR: rename
pub fn rename(
    sess: &mut Session,
    program: ast::Program,
) -> Result<Program, Stop> {
    let mut program = program;
    let mut prelude_file = None;
    let mut prelude_fixities = vec![];
    if !sess.options.no_prelude {
        let file = sess
            .sources
            .add("<prelude>", include_str!("../prelude.fun").into());
        prelude_file = Some(file);
        sess.prelude_file = Some(file);
        let prelude = crate::parse::parse(sess, file)?;
        prelude_fixities = prelude.fixities;
        let mut bindings = prelude.bindings;
        bindings.extend(program.bindings);
        program.bindings = bindings;
        program.data.splice(0..0, prelude.data);
        program.classes.splice(0..0, prelude.classes);
        program.instances.splice(0..0, prelude.instances);
        program.aliases.splice(0..0, prelude.aliases);
    }
    let mut data =
        crate::data::collect(sess, &program.data, &program.aliases)?;
    let mut classes = match crate::classes::collect(&mut program, &mut data)
    {
        Ok(c) => c,
        Err(e) => {
            sess.emit(e);
            return Err(Stop);
        }
    };
    let (prelude_bindings, user_bindings): (Vec<_>, Vec<_>) =
        std::mem::take(&mut program.bindings)
            .into_iter()
            .partition(|b| Some(b.span.file) == prelude_file);
    let mut prelude_program = program.clone();
    prelude_program.bindings = prelude_bindings;
    prelude_program.fixities = prelude_fixities.clone();
    let prelude_program = crate::fixity::resolve(sess, prelude_program)?;
    program.bindings = user_bindings;
    let program = crate::fixity::resolve_with_prelude(
        sess,
        program,
        &prelude_fixities,
    )?;
    for fixity in &program.fixities {
        for op in &fixity.operators {
            if !program.bindings.iter().any(|b| b.name == *op)
                && !classes.classes.iter().any(|c| {
                    c.span.file == program.span.file
                        && c.methods.iter().any(|m| m.name == *op)
                })
            {
                sess.emit(Diagnostic::error(
                    ErrorCode::OrphanFixity,
                    format!("fixity without a definition: {op}"),
                    fixity.span,
                ));
            }
        }
    }
    let mut primitives = Vec::new();
    let mut prelude = BTreeMap::new();
    if !sess.options.no_prelude {
        for p in crate::primitives::ALL {
            let op = Op::Runtime(*p);
            let id = sess.fresh_id(p.name());
            prelude.insert(p.name().into(), id.clone());
            primitives.push((op, id));
        }
    }
    for c in &mut classes.classes {
        for m in &mut c.methods {
            let id = sess.fresh_id(&m.name);
            prelude.insert(m.name.clone(), id.clone());
            m.id = Some(id);
        }
    }
    let mut renamer = Renamer {
        prelude_file,
        data: data.clone(),
        sess,
        scopes: vec![prelude],
    };
    let mut groups = renamer.bindings(prelude_program.bindings);
    groups.extend(renamer.bindings(program.bindings));
    if renamer.sess.options.require_io
        && !renamer.scopes.iter().any(|s| s.contains_key("main"))
    {
        renamer.sess.emit(Diagnostic::error(
            ErrorCode::Main,
            "missing main :: IO ()",
            program.span,
        ));
        return Err(Stop);
    }
    let main = renamer.lookup("main", program.span);
    renamer.sess.finish(Program {
        classes,
        data,
        groups,
        primitives,
        main,
        span: program.span,
    })
}
// ANCHOR_END: rename
/// Levenshtein distance uses one previous row, including insertion/deletion/substitution.
pub fn distance(a: &str, b: &str) -> usize {
    let b: Vec<_> = b.chars().collect();
    let mut row: Vec<_> = (0..=b.len()).collect();
    for (i, a) in a.chars().enumerate() {
        let mut diagonal = row[0];
        row[0] = i + 1;
        for (j, &b) in b.iter().enumerate() {
            let old = row[j + 1];
            row[j + 1] = (row[j] + 1)
                .min(old + 1)
                .min(diagonal + usize::from(a != b));
            diagonal = old;
        }
    }
    row[b.len()]
}
