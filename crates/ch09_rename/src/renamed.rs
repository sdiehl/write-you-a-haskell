//! Resolved identifiers and dependency groups form the input to inference.
use crate::{ast::Op, names::Id, source::Span, types::Scheme};
#[derive(Clone, Debug)]
/// Every variable occurrence refers to exactly one uniquely identified binder.
pub struct Expr {
    pub kind: Kind,
    pub span: Span,
}
#[derive(Clone, Debug)]
/// The resolved expression forms, after surface operators and signatures are separated.
pub enum Kind {
    Int(i64),
    Bool(bool),
    Char(char),
    String(String),
    Var(Id),
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
    pub annotation: Option<Scheme>,
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
    pub groups: Vec<BindGroup>,
    pub primitives: Vec<(Op, Id)>,
    pub main: Id,
    pub span: Span,
}
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            Kind::Int(n) => write!(f, "{n}"),
            Kind::Bool(b) => {
                write!(f, "{}", if *b { "True" } else { "False" })
            }
            Kind::Char(c) => {
                write!(f, "{}", crate::ast::quote(&c.to_string(), '\''))
            }
            Kind::String(s) => write!(f, "{}", crate::ast::quote(s, '"')),
            Kind::Var(id) => write!(f, "{id}"),
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
