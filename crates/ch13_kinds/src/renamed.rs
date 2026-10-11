//! Resolved identifiers and dependency groups form the input to inference.
use crate::{ast::Op, ast::TypeSyntax, names::Id, source::Span};
#[derive(Clone, Debug)]
/// Every variable occurrence refers to exactly one uniquely identified binder.
pub struct Expr {
    pub node: u32,
    pub kind: Kind,
    pub span: Span,
}
#[derive(Clone, Debug)]
/// The resolved expression forms, after surface operators and signatures are separated.
pub enum Kind {
    Record(crate::data::ConId, Vec<(String, Expr)>),
    Project(Box<Expr>, String),
    Update(Box<Expr>, Vec<(String, Expr)>),
    Annotated(Box<Expr>, crate::ast::TypeSyntax),
    Int(i64),
    Bool(bool),
    Char(char),
    String(String),
    Var(Id),
    Con(crate::data::ConId),
    PatternLam(Vec<Pat>, Box<Expr>),
    Equations(Vec<Row>),
    Case(Box<Expr>, Vec<Row>),
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    Match(Vec<Expr>, Vec<Row>),
    Lam(Id, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(Vec<BindGroup>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Binary(Op, Box<Expr>, Box<Expr>),
}
#[derive(Clone, Debug)]
/// One uniquely named right-hand side and its optional source signature.
pub struct Binding {
    pub id: Id,
    pub rhs: Expr,
    pub annotation: Option<TypeSyntax>,
    pub span: Span,
}
#[derive(Clone, Debug)]
/// Members of a recursive component are inferred together before generalization.
pub struct BindGroup {
    pub recursive: bool,
    pub binds: Vec<Binding>,
}
#[derive(Clone, Debug)]
/// Dependency-ordered globals, intrinsic identities, and the selected entry point.
pub struct Program {
    pub data: crate::data::DataEnv,
    pub groups: Vec<BindGroup>,
    pub primitives: Vec<(Op, Id)>,
    pub main: Id,
    pub span: Span,
}
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            Kind::Record(id, fields) => {
                write!(f, "(record {}", id.name)?;
                for (n, e) in fields {
                    write!(f, " {n}={e}")?;
                }
                write!(f, ")")
            }
            Kind::Project(e, n) => write!(f, "(project {e} {n})"),
            Kind::Update(e, fields) => {
                write!(f, "(update {e}")?;
                for (n, e) in fields {
                    write!(f, " {n}={e}")?;
                }
                write!(f, ")")
            }
            Kind::Annotated(e, t) => write!(f, "({e} :: {t})"),
            Kind::Int(n) => write!(f, "{n}"),
            Kind::Bool(b) => {
                write!(f, "{}", if *b { "True" } else { "False" })
            }
            Kind::Char(c) => {
                write!(f, "{}", crate::ast::quote(&c.to_string(), '\''))
            }
            Kind::String(s) => write!(f, "{}", crate::ast::quote(s, '"')),
            Kind::Var(id) => write!(f, "{id}"),
            Kind::Con(id) => write!(f, "{}", id.name),
            Kind::PatternLam(ps, e) => {
                write!(f, "(pattern-lambda")?;
                for p in ps {
                    write!(f, " {p}")?;
                }
                write!(f, " {e})")
            }
            Kind::Equations(rows) => {
                write!(f, "(equations")?;
                for row in rows {
                    write!(f, " {row}")?;
                }
                write!(f, ")")
            }
            Kind::Case(e, rows) => {
                write!(f, "(case {e}")?;
                for row in rows {
                    write!(f, " {row}")?;
                }
                write!(f, ")")
            }
            Kind::List(es) | Kind::Tuple(es) => {
                write!(f, "(sugar")?;
                for e in es {
                    write!(f, " {e}")?;
                }
                write!(f, ")")
            }
            Kind::Match(es, rows) => {
                write!(f, "(match")?;
                for e in es {
                    write!(f, " {e}")?;
                }
                for row in rows {
                    write!(f, " {row}")?;
                }
                write!(f, ")")
            }
            Kind::Lam(id, b) => write!(f, "(lambda {id} {b})"),
            Kind::App(a, b) => write!(f, "(app {a} {b})"),
            Kind::If(c, a, b) => write!(f, "(if {c} {a} {b})"),
            Kind::Neg(a) => write!(f, "(neg {a})"),
            Kind::Binary(op, a, b) => write!(f, "({} {a} {b})", op.text()),
            Kind::Let(groups, b) => {
                write!(f, "(let ")?;
                for g in groups {
                    write!(f, "{g} ")?;
                }
                write!(f, "in {b})")
            }
        }
    }
}
impl std::fmt::Display for BindGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{", if self.recursive { "rec" } else { "nonrec" })?;
        for b in &self.binds {
            write!(f, " {} = {};", b.id, b.rhs)?;
        }
        write!(f, " }}")
    }
}
/// Gather referenced unique names. Local binders cannot collide with a group's
/// names, so SCC construction needs no spelling-based shadowing adjustment.
pub fn references(expr: &Expr, out: &mut std::collections::BTreeSet<Id>) {
    match &expr.kind {
        Kind::Annotated(e, _) | Kind::Project(e, _) => references(e, out),
        Kind::Record(_, fields) => {
            for (_, e) in fields {
                references(e, out);
            }
        }
        Kind::Update(e, fields) => {
            references(e, out);
            for (_, e) in fields {
                references(e, out);
            }
        }
        Kind::PatternLam(_, e) => references(e, out),
        Kind::List(es) | Kind::Tuple(es) => {
            for e in es {
                references(e, out);
            }
        }
        Kind::Equations(rows)
        | Kind::Case(_, rows)
        | Kind::Match(_, rows) => {
            if let Kind::Case(e, _) = &expr.kind {
                references(e, out);
            }
            if let Kind::Match(es, _) = &expr.kind {
                for e in es {
                    references(e, out);
                }
            }
            for row in rows {
                for g in &row.locals {
                    for b in &g.binds {
                        references(&b.rhs, out);
                    }
                }
                match &row.body {
                    RowBody::Plain(e) => references(e, out),
                    RowBody::Guarded(gs) => {
                        for (g, e) in gs {
                            references(g, out);
                            references(e, out);
                        }
                    }
                }
            }
        }
        Kind::Var(id) => {
            out.insert(id.clone());
        }
        Kind::Lam(_, a) | Kind::Neg(a) => references(a, out),
        Kind::App(a, b) | Kind::Binary(_, a, b) => {
            references(a, out);
            references(b, out);
        }
        Kind::If(c, a, b) => {
            references(c, out);
            references(a, out);
            references(b, out);
        }
        Kind::Let(groups, body) => {
            for g in groups {
                for b in &g.binds {
                    references(&b.rhs, out);
                }
            }
            references(body, out);
        }
        _ => {}
    }
}

/// Pattern binders have the same unique identities as their RHS occurrences.
#[derive(Clone, Debug)]
pub struct Pat {
    pub kind: PatKind,
    pub span: Span,
}
#[derive(Clone, Debug)]
pub enum PatKind {
    Var(Id),
    Wild,
    Con(crate::data::ConId, Vec<Pat>),
    As(Id, Box<Pat>),
    Int(i64),
    Char(char),
    String(String),
    List(Vec<Pat>),
    Tuple(Vec<Pat>),
}
/// The syntactic guards retain their fallthrough behavior through elaboration.
#[derive(Clone, Debug)]
pub enum RowBody {
    Plain(Box<Expr>),
    Guarded(Vec<(Expr, Expr)>),
}
/// Locals are shared by the guards and RHS after this row's patterns match.
#[derive(Clone, Debug)]
pub struct Row {
    pub patterns: Vec<Pat>,
    pub body: RowBody,
    pub locals: Vec<BindGroup>,
    pub span: Span,
}

impl std::fmt::Display for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            PatKind::Var(id) => write!(f, "{id}"),
            PatKind::Wild => write!(f, "_"),
            PatKind::As(id, p) => write!(f, "{id}@{p}"),
            PatKind::Int(n) => write!(f, "{n}"),
            PatKind::Char(c) => {
                write!(f, "{}", crate::ast::quote(&c.to_string(), '\''))
            }
            PatKind::String(s) => {
                write!(f, "{}", crate::ast::quote(s, '"'))
            }
            PatKind::Con(id, ps) => {
                write!(f, "({}", id.name)?;
                for p in ps {
                    write!(f, " {p}")?;
                }
                write!(f, ")")
            }
            PatKind::List(ps) | PatKind::Tuple(ps) => {
                write!(f, "(sugar")?;
                for p in ps {
                    write!(f, " {p}")?;
                }
                write!(f, ")")
            }
        }
    }
}
impl std::fmt::Display for Row {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for p in &self.patterns {
            write!(f, " {p}")?;
        }
        for group in &self.locals {
            write!(f, " {group}")?;
        }
        match &self.body {
            RowBody::Plain(e) => write!(f, " -> {e}")?,
            RowBody::Guarded(gs) => {
                for (g, e) in gs {
                    write!(f, " | {g} -> {e}")?;
                }
            }
        }
        write!(f, " ]")
    }
}
