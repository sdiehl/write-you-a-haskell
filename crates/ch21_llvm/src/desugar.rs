//! Lower named surface sugar before type inference; guards remain explicit rows.
use crate::{
    data::{ConId, DataEnv},
    diagnostics::{Diagnostic, ErrorCode},
    renamed::{self, BindGroup, Expr, Kind, Pat, PatKind, Row, RowBody},
    session::{Session, Stop},
    source::Span,
    syn,
};
struct Desugar<'a> {
    sess: &'a mut Session,
    data: &'a DataEnv,
}
impl Desugar<'_> {
    fn node(&mut self, kind: Kind, span: Span) -> Expr {
        Expr {
            kind,
            span,
            node: self.sess.fresh_node(),
        }
    }
    fn constructor(&mut self, name: &str, span: Span) -> ConId {
        if let Some(info) = self.data.constructor(name) {
            return info.id.clone();
        }
        self.sess.emit(Diagnostic::error(
            ErrorCode::UnboundConstructor,
            format!("sugar requires constructor {name} from the Prelude"),
            span,
        ));
        ConId {
            index: 0,
            name: name.into(),
        }
    }
    fn tuple(&mut self, n: usize, span: Span) -> ConId {
        if !matches!(n, 0 | 2 | 3) {
            self.sess.emit(Diagnostic::error(
                ErrorCode::Parse,
                "only unit, pairs, and triples are supported",
                span,
            ));
        }
        self.constructor(
            &format!("({})", ",".repeat(n.saturating_sub(1))),
            span,
        )
    }
    fn pattern(&mut self, p: Pat) -> Pat {
        let span = p.span;
        let kind = match p.kind {
            PatKind::List(ps) => {
                let nil = self.constructor("[]", span);
                let cons = self.constructor(":", span);
                let mut tail = Pat {
                    kind: PatKind::Con(nil, vec![]),
                    span,
                };
                for p in ps.into_iter().rev() {
                    tail = Pat {
                        kind: PatKind::Con(
                            cons.clone(),
                            vec![self.pattern(p), tail],
                        ),
                        span,
                    };
                }
                return tail;
            }
            PatKind::Tuple(ps) => PatKind::Con(
                self.tuple(ps.len(), span),
                ps.into_iter().map(|p| self.pattern(p)).collect(),
            ),
            PatKind::Con(id, ps) => PatKind::Con(
                id,
                ps.into_iter().map(|p| self.pattern(p)).collect(),
            ),
            PatKind::As(id, p) => {
                PatKind::As(id, Box::new(self.pattern(*p)))
            }
            PatKind::Int(n) => PatKind::Int((n << 1) >> 1),
            other => other,
        };
        Pat { kind, span }
    }
    fn groups(&mut self, groups: Vec<BindGroup>) -> Vec<BindGroup> {
        groups
            .into_iter()
            .map(|mut g| {
                for b in &mut g.binds {
                    b.rhs = self.expression(b.rhs.clone());
                }
                g
            })
            .collect()
    }
    fn row(&mut self, row: Row) -> Row {
        let body = match row.body {
            RowBody::Plain(e) => {
                RowBody::Plain(Box::new(self.expression(*e)))
            }
            RowBody::Guarded(gs) => RowBody::Guarded(
                gs.into_iter()
                    .map(|(g, e)| (self.expression(g), self.expression(e)))
                    .collect(),
            ),
        };
        Row {
            patterns: row
                .patterns
                .into_iter()
                .map(|p| self.pattern(p))
                .collect(),
            body,
            locals: self.groups(row.locals),
            span: row.span,
        }
    }
    fn equations(&mut self, rows: Vec<Row>, span: Span) -> Expr {
        let mut rows: Vec<_> =
            rows.into_iter().map(|r| self.row(r)).collect();
        if rows.len() == 1
            && rows[0]
                .patterns
                .iter()
                .all(|p| matches!(p.kind, PatKind::Var(_)))
            && matches!(rows[0].body, RowBody::Plain(_))
        {
            let row = rows.pop().unwrap();
            let RowBody::Plain(body) = row.body else {
                unreachable!()
            };
            let mut body = *body;
            if !row.locals.is_empty() {
                body =
                    self.node(Kind::Let(row.locals, Box::new(body)), span);
            }
            for p in row.patterns.into_iter().rev() {
                let PatKind::Var(id) = p.kind else {
                    unreachable!()
                };
                body = self.node(Kind::Lam(id, Box::new(body)), span);
            }
            return body;
        }
        // ANCHOR: equations-match
        let params: Vec<_> = (0..rows[0].patterns.len())
            .map(|_| self.sess.fresh_id("$match"))
            .collect();
        let scruts = params
            .iter()
            .map(|id| self.node(Kind::Var(id.clone()), span))
            .collect();
        let mut body = self.node(Kind::Match(scruts, rows), span);
        for id in params.into_iter().rev() {
            body = self.node(Kind::Lam(id, Box::new(body)), span);
        }
        body
        // ANCHOR_END: equations-match
    }
    fn expression(&mut self, expr: Expr) -> Expr {
        let mut work = vec![(Some(expr), None)];
        let mut values: Vec<Expr> = Vec::new();
        while let Some((expr, saved)) = work.pop() {
            match expr {
                Some(Expr {
                    kind: Kind::App(f, a),
                    node,
                    span,
                }) => {
                    work.push((None, Some((node, span))));
                    work.push((Some(*a), None));
                    work.push((Some(*f), None));
                }
                Some(expr) => values.push(self.expression_leaf(expr)),
                None => {
                    let a = values.pop().expect("application argument");
                    let f = values.pop().expect("application function");
                    let (node, span) = saved.expect("application metadata");
                    values.push(Expr {
                        node,
                        span,
                        kind: Kind::App(Box::new(f), Box::new(a)),
                    });
                }
            }
        }
        values.pop().expect("desugared root")
    }
    fn expression_leaf(&mut self, expr: Expr) -> Expr {
        let span = expr.span;
        let kind = match expr.kind {
            Kind::Record(id, fields) => Kind::Record(
                id,
                fields
                    .into_iter()
                    .map(|(n, e)| (n, self.expression(e)))
                    .collect(),
            ),
            Kind::Project(e, n) => {
                Kind::Project(Box::new(self.expression(*e)), n)
            }
            Kind::Update(e, fields) => Kind::Update(
                Box::new(self.expression(*e)),
                fields
                    .into_iter()
                    .map(|(n, e)| (n, self.expression(e)))
                    .collect(),
            ),
            Kind::Annotated(e, t) => {
                Kind::Annotated(Box::new(self.expression(*e)), t)
            }
            Kind::Bool(b) => Kind::Con(
                self.constructor(if b { "True" } else { "False" }, span),
            ),
            Kind::Equations(rows) => return self.equations(rows, span),
            Kind::PatternLam(patterns, body) => {
                return self.equations(
                    vec![Row {
                        patterns,
                        body: RowBody::Plain(body),
                        locals: vec![],
                        span,
                    }],
                    span,
                );
            }
            Kind::Case(e, rows) => Kind::Match(
                vec![self.expression(*e)],
                rows.into_iter().map(|r| self.row(r)).collect(),
            ),
            Kind::If(c, a, b) => {
                let c = self.expression(*c);
                let a = self.expression(*a);
                let b = self.expression(*b);
                let rows = [("True", a), ("False", b)]
                    .into_iter()
                    .map(|(name, body)| Row {
                        patterns: vec![Pat {
                            kind: PatKind::Con(
                                self.constructor(name, span),
                                vec![],
                            ),
                            span,
                        }],
                        body: RowBody::Plain(Box::new(body)),
                        locals: vec![],
                        span,
                    })
                    .collect();
                Kind::Match(vec![c], rows)
            }
            Kind::List(es) => {
                let nil = self.constructor("[]", span);
                let cons = self.constructor(":", span);
                let mut tail = self.node(Kind::Con(nil), span);
                for e in es.into_iter().rev() {
                    let con = self.node(Kind::Con(cons.clone()), span);
                    let head = self.expression(e);
                    let app = self.node(
                        Kind::App(Box::new(con), Box::new(head)),
                        span,
                    );
                    tail = self.node(
                        Kind::App(Box::new(app), Box::new(tail)),
                        span,
                    );
                }
                return tail;
            }
            Kind::Tuple(es) => {
                let id = self.tuple(es.len(), span);
                let mut value = self.node(Kind::Con(id), span);
                for e in es {
                    let e = self.expression(e);
                    value = self.node(
                        Kind::App(Box::new(value), Box::new(e)),
                        span,
                    );
                }
                return value;
            }
            Kind::Lam(id, b) => {
                Kind::Lam(id, Box::new(self.expression(*b)))
            }
            Kind::App(a, b) => Kind::App(
                Box::new(self.expression(*a)),
                Box::new(self.expression(*b)),
            ),
            Kind::Let(gs, b) => {
                Kind::Let(self.groups(gs), Box::new(self.expression(*b)))
            }
            Kind::Neg(a) => Kind::Neg(Box::new(self.expression(*a))),
            Kind::Binary(op, a, b) => Kind::Binary(
                op,
                Box::new(self.expression(*a)),
                Box::new(self.expression(*b)),
            ),
            Kind::Match(..) => {
                unreachable!("Match is produced by this pass")
            }
            other => other,
        };
        Expr {
            kind,
            span,
            node: expr.node,
        }
    }
}
/// Produce Syn after names are resolved, preserving row scopes and guard order.
pub fn desugar(
    sess: &mut Session,
    program: renamed::Program,
) -> Result<syn::Program, Stop> {
    let groups = {
        let mut pass = Desugar {
            sess,
            data: &program.data,
        };
        pass.groups(program.groups)
    };
    sess.finish(syn::Program {
        classes: program.classes,
        data: program.data,
        groups,
        primitives: program.primitives,
        main: program.main,
        span: program.span,
    })
}

/// Statements are lowered before name resolution so ordinary lexical scopes apply.
pub enum Statement {
    Expr(crate::ast::Expr),
    Bind(crate::ast::Pat, crate::ast::Expr),
    Let(Vec<crate::ast::Binding>),
}
/// Expand do syntax into ordinary applications, matches, and local bindings.
pub fn do_block(
    statements: Vec<Statement>,
    span: Span,
) -> Result<crate::ast::Expr, Diagnostic> {
    use crate::ast::{
        Expr as E, Kind as K, Pat as P, PatKind as PK, Row as R,
        RowBody as B,
    };
    let node = |kind| E { kind, span };
    let app = |a, b| node(K::App(Box::new(a), Box::new(b)));
    let mut statements = statements;
    let Some(Statement::Expr(mut body)) = statements.pop() else {
        return Err(Diagnostic::error(
            ErrorCode::Parse,
            "do must end with an expression",
            span,
        ));
    };
    for statement in statements.into_iter().rev() {
        body = match statement {
            Statement::Expr(e) => {
                app(app(node(K::Var(">>".into())), e), body)
            }
            Statement::Let(bs) => node(K::LetGroup(bs, Box::new(body))),
            Statement::Bind(p, e) => {
                let continuation = match &p.kind {
                    PK::Var(n) => node(K::Lam(n.clone(), Box::new(body))),
                    PK::Wild => node(K::Lam("$do".into(), Box::new(body))),
                    _ => {
                        let site = p.span;
                        let rows = vec![
                            R {
                                patterns: vec![p],
                                body: B::Plain(Box::new(body)),
                                locals: vec![],
                                span: site,
                            },
                            R {
                                patterns: vec![P {
                                    kind: PK::Wild,
                                    span: site,
                                }],
                                body: B::Plain(Box::new(E {
                                    kind: K::DoFail,
                                    span: site,
                                })),
                                locals: vec![],
                                span: site,
                            },
                        ];
                        node(K::Lam(
                            "$do".into(),
                            Box::new(node(K::Case(
                                Box::new(node(K::Var("$do".into()))),
                                rows,
                            ))),
                        ))
                    }
                };
                app(app(node(K::Var(">>=".into())), e), continuation)
            }
        };
    }
    Ok(body)
}

pub fn do_pattern(
    e: crate::ast::Expr,
) -> Result<crate::ast::Pat, Diagnostic> {
    use crate::ast::{Kind as K, Pat as P, PatKind as PK};
    let span = e.span;
    let kind = match e.kind {
        K::Chain(a, pairs, None) if pairs.is_empty() => {
            return do_pattern(*a);
        }
        K::Chain(a, pairs, None)
            if pairs.iter().all(|(op, _)| op == ":") =>
        {
            let mut elements = vec![do_pattern(*a)?];
            for (_, e) in pairs {
                elements.push(do_pattern(e)?);
            }
            let mut tail = elements.pop().unwrap();
            for head in elements.into_iter().rev() {
                tail = P {
                    kind: PK::Con(":".into(), vec![head, tail]),
                    span,
                };
            }
            return Ok(tail);
        }
        K::PatternAs(n, p) => PK::As(n, Box::new(do_pattern(*p)?)),
        K::Paren(a) => return do_pattern(*a),
        K::Var(n) if n == "_" => PK::Wild,
        K::Var(n) if n.starts_with(char::is_uppercase) => {
            PK::Con(n, vec![])
        }
        K::Var(n) => PK::Var(n),
        K::App(a, b) => {
            let mut p = do_pattern(*a)?;
            if let PK::Con(_, ps) = &mut p.kind {
                ps.push(do_pattern(*b)?);
                return Ok(p);
            } else {
                return Err(Diagnostic::error(
                    ErrorCode::Parse,
                    "invalid do pattern",
                    span,
                ));
            }
        }
        K::Int(n) => PK::Int(n),
        K::Double(n) => PK::Double(n),
        K::Char(c) => PK::Char(c),
        K::String(s) => PK::String(s),
        K::Neg(a) => {
            if let K::Int(n) = a.kind {
                PK::Int(-n)
            } else {
                return Err(Diagnostic::error(
                    ErrorCode::Parse,
                    "invalid negative pattern",
                    span,
                ));
            }
        }
        K::List(es) => PK::List(
            es.into_iter().map(do_pattern).collect::<Result<_, _>>()?,
        ),
        K::Tuple(es) => PK::Tuple(
            es.into_iter().map(do_pattern).collect::<Result<_, _>>()?,
        ),
        K::Update(a, fs) => {
            fn record(
                mut p: P,
                fields: Vec<(String, P)>,
            ) -> Result<P, Diagnostic> {
                p.kind = match p.kind {
                    PK::As(n, p) => {
                        PK::As(n, Box::new(record(*p, fields)?))
                    }
                    PK::Con(n, ps) if ps.is_empty() => {
                        PK::Record(n, fields)
                    }
                    _ => {
                        return Err(Diagnostic::error(
                            ErrorCode::Parse,
                            "invalid record pattern",
                            p.span,
                        ));
                    }
                };
                Ok(p)
            }
            let fields = fs
                .into_iter()
                .map(|(n, e)| Ok((n, do_pattern(e)?)))
                .collect::<Result<_, Diagnostic>>()?;
            return record(do_pattern(*a)?, fields);
        }
        K::Record(n, fs) => PK::Record(
            n,
            fs.into_iter()
                .map(|(n, e)| Ok((n, do_pattern(e)?)))
                .collect::<Result<_, Diagnostic>>()?,
        ),
        _ => {
            return Err(Diagnostic::error(
                ErrorCode::Parse,
                "invalid do pattern",
                span,
            ));
        }
    };
    Ok(P { kind, span })
}

/// Inclusive Int ranges are ordinary calls to the finite Prelude enumerator.
pub fn range(
    a: crate::ast::Expr,
    b: crate::ast::Expr,
    span: crate::source::Span,
) -> crate::ast::Expr {
    use crate::ast::{Expr, Kind};
    let head = Expr {
        kind: Kind::Var("enumFromTo".into()),
        span,
    };
    let call = Expr {
        kind: Kind::App(Box::new(head), Box::new(a)),
        span,
    };
    Expr {
        kind: Kind::App(Box::new(call), Box::new(b)),
        span,
    }
}
