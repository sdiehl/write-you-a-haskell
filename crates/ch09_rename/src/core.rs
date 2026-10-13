//! Untyped lambda Core keeps runtime operations separate from source syntax.
use crate::{ast::Op, names::Id, source::Span};

// ANCHOR: core
#[derive(Clone, Debug, PartialEq, Eq)]
/// Immediate source values; strings are primitive UTF-8 values, not character lists.
pub enum Lit {
    Int(i64),
    Bool(bool),
    Char(char),
    String(String),
}

#[derive(Clone, Debug)]
/// The initial lambda Core; types, data constructors, and compiled cases arrive later.
pub enum Expr {
    Var(Id),
    Lit(Lit),
    Lam(Id, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(BindGroup, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Prim(Op, Vec<Expr>, Span),
}

#[derive(Clone, Debug)]
/// A recursive binding is guaranteed to have a lambda right-hand side.
pub struct Bind {
    pub name: Id,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug)]
/// An ordered component; recursive members are in scope in every right-hand side.
pub struct BindGroup {
    pub recursive: bool,
    pub binds: Vec<Bind>,
}

#[derive(Clone, Debug)]
/// Globals are initialized in dependency order, with explicit recursive components.
pub struct Program {
    pub groups: Vec<BindGroup>,
    pub main: Id,
}
// ANCHOR_END: core

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(n) => write!(f, "{n}"),
            Self::Lit(l) => write!(f, "{l}"),
            Self::Lam(n, b) => write!(f, "(lambda {n} {b})"),
            Self::App(a, b) => write!(f, "(app {a} {b})"),
            Self::Let(group, body) => write!(f, "(let {group} in {body})"),
            Self::If(c, a, b) => write!(f, "(if {c} {a} {b})"),
            Self::Prim(op, args, _) => {
                write!(f, "(prim {}", op.text())?;
                for a in args {
                    write!(f, " {a}")?;
                }
                write!(f, ")")
            }
        }
    }
}

impl std::fmt::Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{n}"),
            Self::Bool(b) => {
                write!(f, "{}", if *b { "True" } else { "False" })
            }
            Self::Char(c) => {
                write!(f, "{}", crate::ast::quote(&c.to_string(), '\''))
            }
            Self::String(s) => write!(f, "{}", crate::ast::quote(s, '"')),
        }
    }
}

impl std::fmt::Display for BindGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{", if self.recursive { "rec" } else { "nonrec" })?;
        for b in &self.binds {
            write!(f, " {} = {};", b.name, b.rhs)?;
        }
        write!(f, " }}")
    }
}

/// Check lexical scope, unique binders, primitive saturation, and recursive lambdas.
pub fn lint(program: &Program) -> Result<(), String> {
    use std::collections::BTreeSet;
    fn binder(id: &Id, seen: &mut BTreeSet<u32>) -> Result<(), String> {
        if !seen.insert(id.name.unique) {
            Err(format!("duplicate Core binder {id}"))
        } else {
            Ok(())
        }
    }
    fn expr(
        e: &Expr,
        scope: &mut Vec<Id>,
        seen: &mut BTreeSet<u32>,
    ) -> Result<(), String> {
        match e {
            Expr::Var(id) if !scope.contains(id) => {
                Err(format!("unbound Core variable {id}"))
            }
            Expr::Var(_) | Expr::Lit(_) => Ok(()),
            Expr::Lam(id, body) => {
                binder(id, seen)?;
                scope.push(id.clone());
                let result = expr(body, scope, seen);
                scope.pop();
                result
            }
            Expr::App(a, b) => {
                expr(a, scope, seen)?;
                expr(b, scope, seen)
            }
            Expr::If(c, a, b) => {
                expr(c, scope, seen)?;
                expr(a, scope, seen)?;
                expr(b, scope, seen)
            }
            Expr::Let(bindings, body) => {
                let n = scope.len();
                group(bindings, scope, seen)?;
                let result = expr(body, scope, seen);
                scope.truncate(n);
                result
            }
            Expr::Prim(_, args, _) => {
                if args.len() != 2 {
                    return Err("unsaturated primitive".into());
                }
                for a in args {
                    expr(a, scope, seen)?;
                }
                Ok(())
            }
        }
    }
    fn group(
        group: &BindGroup,
        scope: &mut Vec<Id>,
        seen: &mut BTreeSet<u32>,
    ) -> Result<(), String> {
        for b in &group.binds {
            binder(&b.name, seen)?;
        }
        if group.recursive {
            for b in &group.binds {
                if !matches!(*b.rhs, Expr::Lam(..)) {
                    return Err("recursive non-lambda".into());
                }
                scope.push(b.name.clone());
            }
        }
        for b in &group.binds {
            expr(&b.rhs, scope, seen)?;
        }
        if !group.recursive {
            for b in &group.binds {
                scope.push(b.name.clone());
            }
        }
        Ok(())
    }
    let mut scope = vec![];
    let mut seen = BTreeSet::new();
    for g in &program.groups {
        group(g, &mut scope, &mut seen)?;
    }
    if !scope.contains(&program.main) {
        return Err("missing main".into());
    }
    Ok(())
}
