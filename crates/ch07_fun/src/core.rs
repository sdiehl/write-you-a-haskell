//! Untyped lambda Core keeps runtime operations separate from source syntax.
use crate::{ast::Op, source::Span};

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
    Var(String),
    Lit(Lit),
    Lam(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(Bind, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Prim(Op, Vec<Expr>, Span),
}

#[derive(Clone, Debug)]
/// A recursive binding is guaranteed to have a lambda right-hand side.
pub struct Bind {
    pub name: String,
    pub rhs: Box<Expr>,
    pub recursive: bool,
}

#[derive(Clone, Debug)]
/// Globals retain source order in this chapter; SCC ordering arrives in Chapter 9.
pub struct Program {
    pub bindings: Vec<Bind>,
    pub main: String,
}
// ANCHOR_END: core

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(n) => write!(f, "{n}"),
            Self::Lit(l) => write!(f, "{l}"),
            Self::Lam(n, b) => write!(f, "(lambda {n} {b})"),
            Self::App(a, b) => write!(f, "(app {a} {b})"),
            Self::Let(b, body) => write!(
                f,
                "(let{} {} {} {body})",
                if b.recursive { "rec" } else { "" },
                b.name,
                b.rhs
            ),
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

/// Check untyped Core's lexical scope, primitive arity, and recursive-lambda invariant.
/// Chapter 10 replaces these structural checks with full System F lint.
pub fn lint(program: &Program) -> Result<(), String> {
    fn expr(e: &Expr, scope: &mut Vec<String>) -> Result<(), String> {
        match e {
            Expr::Var(n) if !scope.contains(n) => {
                Err(format!("unbound Core variable {n}"))
            }
            Expr::Var(_) | Expr::Lit(_) => Ok(()),
            Expr::Lam(n, b) => {
                scope.push(n.clone());
                let result = expr(b, scope);
                scope.pop();
                result
            }
            Expr::App(a, b) => {
                expr(a, scope)?;
                expr(b, scope)
            }
            Expr::Let(b, body) => {
                binding(b, scope)?;
                scope.push(b.name.clone());
                let result = expr(body, scope);
                scope.pop();
                result
            }
            Expr::If(c, a, b) => {
                expr(c, scope)?;
                expr(a, scope)?;
                expr(b, scope)
            }
            Expr::Prim(_, args, _) => {
                if args.len() != 2 {
                    return Err("unsaturated primitive".into());
                }
                for a in args {
                    expr(a, scope)?;
                }
                Ok(())
            }
        }
    }
    fn binding(b: &Bind, scope: &mut Vec<String>) -> Result<(), String> {
        if b.recursive {
            if !matches!(*b.rhs, Expr::Lam(..)) {
                return Err("recursive non-lambda".into());
            }
            scope.push(b.name.clone());
        }
        let result = expr(&b.rhs, scope);
        if b.recursive {
            scope.pop();
        }
        result
    }
    let mut scope = vec![];
    for b in &program.bindings {
        binding(b, &mut scope)?;
        scope.push(b.name.clone());
    }
    if !scope.contains(&program.main) {
        return Err("missing main".into());
    }
    Ok(())
}
