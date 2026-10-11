//! A small, source-located arithmetic language.
use std::{fmt, ops::Range};

// ANCHOR: ast
/// Byte offsets into the single source file, including parentheses when present.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr {
    pub kind: Kind,
    pub span: Range<usize>,
}

/// An error node exists only during recovery; the driver never evaluates it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    Int(i64),
    Var(String),
    Neg(Box<Expr>),
    Binary(Op, Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Error,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}
// ANCHOR_END: ast

impl Expr {
    pub fn new(kind: Kind, start: usize, end: usize) -> Self {
        Self {
            kind,
            span: start..end,
        }
    }
    pub fn binary(op: Op, left: Self, right: Self) -> Self {
        let (start, end) = (left.span.start, right.span.end);
        Self::new(
            Kind::Binary(op, Box::new(left), Box::new(right)),
            start,
            end,
        )
    }
}
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            Kind::Int(n) => write!(f, "{n}"),
            Kind::Var(name) => write!(f, "{name}"),
            Kind::Neg(value) => write!(f, "(neg {value})"),
            Kind::Binary(op, left, right) => {
                let op = match op {
                    Op::Add => "+",
                    Op::Sub => "-",
                    Op::Mul => "*",
                    Op::Div => "/",
                };
                write!(f, "({op} {left} {right})")
            }
            Kind::Let(name, value, body) => {
                write!(f, "(let {name} {value} {body})")
            }
            Kind::Error => write!(f, "<error>"),
        }
    }
}
