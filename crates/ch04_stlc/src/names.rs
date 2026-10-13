//! Free variables, capture avoidance, and nameless terms.
use crate::ast::{Expr, Kind, Op};
use crate::types::Type;
use std::collections::BTreeSet;

/// Collect free names; a let binder scopes only over its body.
pub fn free(expr: &Expr) -> BTreeSet<String> {
    match &expr.kind {
        Kind::Var(name) => BTreeSet::from([name.clone()]),
        Kind::Lam(name, _, body) => {
            let mut names = free(body);
            names.remove(name);
            names
        }
        Kind::Let(name, value, body) => {
            let mut names = free(body);
            names.remove(name);
            names.extend(free(value));
            names
        }
        Kind::App(a, b) | Kind::Binary(_, a, b) => {
            let mut names = free(a);
            names.extend(free(b));
            names
        }
        Kind::If(c, a, b) => {
            let mut names = free(c);
            names.extend(free(a));
            names.extend(free(b));
            names
        }
        Kind::Neg(a) => free(a),
        _ => BTreeSet::new(),
    }
}

fn all_names(expr: &Expr, names: &mut BTreeSet<String>) {
    match &expr.kind {
        Kind::Var(n) => {
            names.insert(n.clone());
        }
        Kind::Lam(n, _, a) => {
            names.insert(n.clone());
            all_names(a, names);
        }
        Kind::Let(n, a, b) => {
            names.insert(n.clone());
            all_names(a, names);
            all_names(b, names);
        }
        Kind::App(a, b) | Kind::Binary(_, a, b) => {
            all_names(a, names);
            all_names(b, names);
        }
        Kind::If(c, a, b) => {
            all_names(c, names);
            all_names(a, names);
            all_names(b, names);
        }
        Kind::Neg(a) => all_names(a, names),
        _ => {}
    }
}

// ANCHOR: binder
fn binder(
    name: &str,
    body: &Expr,
    target: &str,
    value: &Expr,
) -> (String, Expr) {
    if name == target {
        return (name.to_owned(), body.clone());
    }
    let mut name = name.to_owned();
    let mut body = body.clone();
    if free(value).contains(&name) && free(&body).contains(target) {
        let mut used = BTreeSet::from([target.to_owned()]);
        all_names(&body, &mut used);
        all_names(value, &mut used);
        let mut fresh = format!("{name}'");
        while used.contains(&fresh) {
            fresh.push('\'');
        }
        let variable = Expr::new(Kind::Var(fresh.clone()), 0, 0);
        body = substitute(&body, &name, &variable);
        name = fresh;
    }
    (name, substitute(&body, target, value))
}
// ANCHOR_END: binder

/// Substitute a term for its free occurrences, renaming conflicting binders first.
pub fn substitute(expr: &Expr, target: &str, value: &Expr) -> Expr {
    let sub = |e: &Expr| Box::new(substitute(e, target, value));
    let kind = match &expr.kind {
        Kind::Var(n) if n == target => return value.clone(),
        Kind::Lam(n, ty, body) => {
            let (n, body) = binder(n, body, target, value);
            Kind::Lam(n, ty.clone(), Box::new(body))
        }
        Kind::Let(n, a, b) => {
            let (n, body) = binder(n, b, target, value);
            Kind::Let(n, sub(a), Box::new(body))
        }
        Kind::App(a, b) => Kind::App(sub(a), sub(b)),
        Kind::Binary(op, a, b) => Kind::Binary(*op, sub(a), sub(b)),
        Kind::If(c, a, b) => Kind::If(sub(c), sub(a), sub(b)),
        Kind::Neg(a) => Kind::Neg(sub(a)),
        other => other.clone(),
    };
    Expr::new(kind, expr.span.start, expr.span.end)
}

// ANCHOR: debruijn
/// Bound names are distances to binders; free names remain explicit.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Bound(usize),
    Free(String),
    Int(i64),
    Bool(bool),
    Lam(Type, Box<Term>),
    App(Box<Term>, Box<Term>),
    Let(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Neg(Box<Term>),
    Binary(Op, Box<Term>, Box<Term>),
    Error,
}

/// Convert without substitution; alpha-equivalent expressions have equal terms.
pub fn debruijn(expr: &Expr, bound: &mut Vec<String>) -> Term {
    let mut go = |e: &Expr| Box::new(debruijn(e, bound));
    match &expr.kind {
        Kind::Var(n) => bound
            .iter()
            .rev()
            .position(|x| x == n)
            .map_or_else(|| Term::Free(n.clone()), Term::Bound),
        Kind::Lam(n, ty, a) => {
            bound.push(n.clone());
            let body = debruijn(a, bound);
            bound.pop();
            Term::Lam(ty.clone(), Box::new(body))
        }
        Kind::Let(n, a, b) => {
            let value = go(a);
            bound.push(n.clone());
            let body = debruijn(b, bound);
            bound.pop();
            Term::Let(value, Box::new(body))
        }
        Kind::App(a, b) => Term::App(go(a), go(b)),
        Kind::Binary(op, a, b) => Term::Binary(*op, go(a), go(b)),
        Kind::If(c, a, b) => Term::If(go(c), go(a), go(b)),
        Kind::Neg(a) => Term::Neg(go(a)),
        Kind::Int(n) => Term::Int(*n),
        Kind::Bool(b) => Term::Bool(*b),
        Kind::Error => Term::Error,
    }
}
// ANCHOR_END: debruijn
