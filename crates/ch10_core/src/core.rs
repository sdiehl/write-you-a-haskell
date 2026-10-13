//! Untyped lambda Core keeps runtime operations separate from source syntax.
pub use crate::core_type::Type;
use crate::{ast::Op, source::Span};

/// A Core identifier carries its independently checkable type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Id {
    pub name: crate::names::Id,
    pub ty: Type,
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} : {}", self.name, self.ty)
    }
}

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
/// System F Core makes type abstraction and instantiation explicit.
pub enum Expr {
    Var(Id),
    Global(Id),
    TyLam(u32, Box<Expr>),
    TyApp(Box<Expr>, Type),
    Lit(Lit),
    Lam(Id, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(BindGroup, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Prim(Op, Vec<Type>, Vec<Expr>, Span),
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
            Self::Var(n) => write!(f, "{}", n.name),
            Self::Global(n) => write!(f, "@{}", n.name),
            Self::TyLam(v, b) => write!(f, "(type-lambda t{v} {b})"),
            Self::TyApp(a, t) => write!(f, "(type-app {a} {t})"),
            Self::Lit(l) => write!(f, "{l}"),
            Self::Lam(n, b) => write!(f, "(lambda {n} {b})"),
            Self::App(a, b) => write!(f, "(app {a} {b})"),
            Self::Let(group, body) => write!(f, "(let {group} in {body})"),
            Self::If(c, a, b) => write!(f, "(if {c} {a} {b})"),
            Self::Prim(op, _, args, _) => {
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

/// Independently typecheck System F Core after each Core-producing pass.
pub fn lint(program: &Program) -> Result<(), String> {
    crate::core_lint::lint(program)
}

impl Id {
    pub fn unique(&self) -> u32 {
        self.name.name.unique
    }
}
