//! Named lambda terms and a precedence-aware printer.
use std::{fmt, ops::Range};

// ANCHOR: ast
#[derive(Clone, Debug, PartialEq, Eq)]
/// A named, untyped term with a source span.
pub struct Expr {
    pub kind: Kind,
    pub span: Range<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// The forms of the extended lambda calculus.
pub enum Kind {
    Int(i64),
    Bool(bool),
    Var(String),
    Lam(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Binary(Op, Box<Expr>, Box<Expr>),
    Error,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// Monomorphic integer arithmetic and comparisons.
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Le,
}
// ANCHOR_END: ast

impl Op {
    pub fn text(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Eq => "==",
            Self::Lt => "<",
            Self::Le => "<=",
        }
    }
    pub fn precedence(self) -> u8 {
        match self {
            Self::Eq | Self::Lt | Self::Le => 1,
            Self::Add | Self::Sub => 3,
            Self::Mul | Self::Div => 5,
        }
    }
}

impl Expr {
    pub fn new(kind: Kind, start: usize, end: usize) -> Self {
        Self {
            kind,
            span: start..end,
        }
    }
    pub fn binary(op: Op, a: Self, b: Self) -> Self {
        let (start, end) = (a.span.start, b.span.end);
        Self::new(Kind::Binary(op, Box::new(a), Box::new(b)), start, end)
    }
    fn print(
        &self,
        f: &mut fmt::Formatter<'_>,
        context: u8,
    ) -> fmt::Result {
        let power = match &self.kind {
            Kind::Lam(..) | Kind::Let(..) | Kind::If(..) => 0,
            Kind::Binary(op, ..) => op.precedence(),
            Kind::Neg(..) => 6,
            Kind::Int(n) if *n < 0 => 6,
            Kind::App(..) => 7,
            _ => 9,
        };
        let parens = power < context;
        if parens {
            write!(f, "(")?;
        }
        match &self.kind {
            Kind::Int(n) => write!(f, "{n}")?,
            Kind::Bool(b) => {
                write!(f, "{}", if *b { "True" } else { "False" })?
            }
            Kind::Var(n) => write!(f, "{n}")?,
            Kind::Lam(n, body) => {
                write!(f, "\\{n} -> ")?;
                body.print(f, 0)?;
            }
            Kind::App(a, b) => {
                a.print(f, 7)?;
                write!(f, " ")?;
                b.print(f, 8)?;
            }
            Kind::Let(n, a, b) => {
                write!(f, "let {n} = ")?;
                a.print(f, 0)?;
                write!(f, " in ")?;
                b.print(f, 0)?;
            }
            Kind::If(c, a, b) => {
                write!(f, "if ")?;
                c.print(f, 0)?;
                write!(f, " then ")?;
                a.print(f, 0)?;
                write!(f, " else ")?;
                b.print(f, 0)?;
            }
            Kind::Neg(a) => {
                write!(f, "-")?;
                a.print(f, 6)?;
            }
            Kind::Binary(op, a, b) => {
                // Comparisons are non-associative: parenthesize both nested sides.
                a.print(f, power + u8::from(power == 1))?;
                write!(f, " {} ", op.text())?;
                b.print(f, power + 1)?;
            }
            Kind::Error => write!(f, "<error>")?,
        }
        if parens {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print(f, 0)
    }
}
