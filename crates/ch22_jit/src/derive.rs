//! Derived instances are ordinary syntax, checked by the same instance machinery.
use crate::{
    ast::*,
    diagnostics::{Diagnostic, ErrorCode},
    session::{Session, Stop},
    source::Span,
};
use std::collections::BTreeSet;

struct Builder {
    span: Span,
}
impl Builder {
    fn expr(&self, kind: Kind) -> Expr {
        Expr {
            kind,
            span: self.span,
        }
    }
    fn var(&self, name: &str) -> Expr {
        self.expr(Kind::Var(name.into()))
    }
    fn string(&self, text: &str) -> Expr {
        self.expr(Kind::String(text.into()))
    }
    fn call(&self, name: &str, args: Vec<Expr>) -> Expr {
        args.into_iter().fold(self.var(name), |f, x| {
            self.expr(Kind::App(Box::new(f), Box::new(x)))
        })
    }
    fn pat(&self, name: &str) -> Pat {
        Pat {
            kind: PatKind::Var(name.into()),
            span: self.span,
        }
    }
    fn constructor(&self, c: &ConstructorDecl, prefix: &str) -> Pat {
        Pat {
            kind: PatKind::Con(
                c.name.clone(),
                (0..c.fields.len())
                    .map(|i| self.pat(&format!("{prefix}{i}")))
                    .collect(),
            ),
            span: self.span,
        }
    }
    fn row(&self, patterns: Vec<Pat>, expr: Expr) -> Row {
        Row {
            patterns,
            body: RowBody::Plain(Box::new(expr)),
            locals: vec![],
            span: self.span,
        }
    }
    fn binding(&self, name: &str, rows: Vec<Row>) -> Binding {
        Binding {
            name: name.into(),
            rhs: self.expr(Kind::Equations(rows)),
            nonrecursive: vec![],
            span: self.span,
        }
    }
    fn append(&self, pieces: Vec<Expr>) -> Expr {
        pieces
            .into_iter()
            .reduce(|a, b| self.call("<>", vec![a, b]))
            .unwrap_or_else(|| self.string(""))
    }
    fn eq(&self, d: &DataDecl) -> Binding {
        let mut rows = vec![];
        for (i, a) in d.constructors.iter().enumerate() {
            for (j, b) in d.constructors.iter().enumerate() {
                let mut rhs =
                    self.var(if i == j { "True" } else { "False" });
                if i == j {
                    for k in (0..a.fields.len()).rev() {
                        let test = self.call(
                            "==",
                            vec![
                                self.var(&format!("x{k}")),
                                self.var(&format!("y{k}")),
                            ],
                        );
                        rhs = self.expr(Kind::If(
                            Box::new(test),
                            Box::new(rhs),
                            Box::new(self.var("False")),
                        ));
                    }
                }
                rows.push(self.row(
                    vec![
                        self.constructor(a, "x"),
                        self.constructor(b, "y"),
                    ],
                    rhs,
                ));
            }
        }
        self.binding("==", rows)
    }
    fn ord(&self, d: &DataDecl) -> Binding {
        let mut rows = vec![];
        for (i, a) in d.constructors.iter().enumerate() {
            for (j, b) in d.constructors.iter().enumerate() {
                let mut rhs = self.var(if i < j {
                    "LT"
                } else if i > j {
                    "GT"
                } else {
                    "EQ"
                });
                if i == j {
                    for k in (0..a.fields.len()).rev() {
                        let comparison = self.call(
                            "compare",
                            vec![
                                self.var(&format!("x{k}")),
                                self.var(&format!("y{k}")),
                            ],
                        );
                        let equal = Pat {
                            kind: PatKind::Con("EQ".into(), vec![]),
                            span: self.span,
                        };
                        rhs = self.expr(Kind::Case(
                            Box::new(comparison),
                            vec![
                                self.row(vec![equal], rhs),
                                self.row(
                                    vec![self.pat("order")],
                                    self.var("order"),
                                ),
                            ],
                        ));
                    }
                }
                rows.push(self.row(
                    vec![
                        self.constructor(a, "x"),
                        self.constructor(b, "y"),
                    ],
                    rhs,
                ));
            }
        }
        self.binding("compare", rows)
    }
    fn show(&self, d: &DataDecl) -> Vec<Binding> {
        let mut rows = vec![];
        for c in &d.constructors {
            let mut pieces = vec![self.string(&c.name)];
            if let Some(labels) = &c.labels {
                pieces.push(self.string(" {"));
                for (i, label) in labels.iter().enumerate() {
                    pieces.push(self.string(&format!(
                        "{}{label} = ",
                        if i == 0 { "" } else { ", " }
                    )));
                    pieces.push(
                        self.call("show", vec![self.var(&format!("x{i}"))]),
                    );
                }
                pieces.push(self.string("}"));
            } else {
                for i in 0..c.fields.len() {
                    pieces.push(self.string(" "));
                    pieces.push(self.call(
                        "_showsPrec",
                        vec![
                            self.expr(Kind::Int(11)),
                            self.var(&format!("x{i}")),
                        ],
                    ));
                }
            }
            let mut rhs = self.append(pieces);
            if !c.fields.is_empty() {
                let test = self.call(
                    ">",
                    vec![self.var("p"), self.expr(Kind::Int(10))],
                );
                rhs = self.call("_showParen", vec![test, rhs]);
            }
            rows.push(
                self.row(
                    vec![self.pat("p"), self.constructor(c, "x")],
                    rhs,
                ),
            );
        }
        let show = self.binding(
            "show",
            vec![self.row(
                vec![self.pat("value")],
                self.call(
                    "_showsPrec",
                    vec![self.expr(Kind::Int(0)), self.var("value")],
                ),
            )],
        );
        vec![show, self.binding("_showsPrec", rows)]
    }
}
fn variables(ty: &TypeSyntax, out: &mut BTreeSet<String>) {
    match ty {
        TypeSyntax::Name(n) => {
            out.insert(n.clone());
        }
        TypeSyntax::App(a, b) | TypeSyntax::Arrow(a, b) => {
            variables(a, out);
            variables(b, out);
        }
        TypeSyntax::Qualified(_, t) => variables(t, out),
    }
}
/// Generate only Eq, Ord and Show. Generated terms retain the declaration span.
pub fn derive(
    sess: &mut Session,
    program: &mut Program,
) -> Result<(), Stop> {
    for d in &program.data {
        if d.deriving.is_empty() {
            continue;
        }
        let build = Builder { span: d.span };
        let mut used = BTreeSet::new();
        for c in &d.constructors {
            for ty in &c.fields {
                let expanded =
                    crate::kinds::expand(ty, &program.aliases, d.span)
                        .map_err(|e| {
                            sess.emit(e);
                            Stop
                        })?;
                variables(&expanded, &mut used);
            }
        }
        for class in &d.deriving {
            let methods = match class.as_str() {
                "Eq" => vec![build.eq(d)],
                "Ord" => vec![build.ord(d)],
                "Show" => build.show(d),
                _ => {
                    sess.emit(Diagnostic::error(
                        ErrorCode::Instance,
                        "only Eq, Ord and Show can be derived",
                        d.span,
                    ));
                    return Err(Stop);
                }
            };
            let context = d
                .parameters
                .iter()
                .filter(|p| used.contains(*p))
                .map(|p| PredicateSyntax {
                    class: class.clone(),
                    variable: p.clone(),
                })
                .collect();
            let head = d.parameters.iter().fold(
                TypeSyntax::Name(d.name.clone()),
                |a, p| {
                    TypeSyntax::App(
                        Box::new(a),
                        Box::new(TypeSyntax::Name(p.clone())),
                    )
                },
            );
            program.instances.push(InstanceDecl {
                class: class.clone(),
                head,
                context,
                methods,
                span: d.span,
            });
        }
    }
    Ok(())
}
