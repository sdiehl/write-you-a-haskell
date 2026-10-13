//! Named lambda terms and a precedence-aware printer.
use crate::source::{FileId, Span};
use std::fmt;

// ANCHOR: ast
#[derive(Clone, Debug, PartialEq, Eq)]
/// A named, unannotated term with a source span.
pub struct Expr {
    pub kind: Kind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// The forms of the extended lambda calculus.
pub enum Kind {
    Int(i64),
    Bool(bool),
    Char(char),
    String(String),
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
    Mod,
    Append,
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
            Self::Div => "div",
            Self::Mod => "mod",
            Self::Append => "++",
            Self::Eq => "==",
            Self::Lt => "<",
            Self::Le => "<=",
        }
    }
    pub fn precedence(self) -> u8 {
        match self {
            Self::Eq | Self::Lt | Self::Le => 1,
            Self::Add | Self::Sub => 3,
            Self::Mul | Self::Div | Self::Mod => 5,
            Self::Append => 2,
        }
    }
}

impl Expr {
    pub fn new(kind: Kind, file: FileId, start: usize, end: usize) -> Self {
        Self {
            kind,
            span: Span::new(file, start, end),
        }
    }
    pub fn binary(op: Op, a: Self, b: Self) -> Self {
        let (file, start, end) =
            (a.span.file, a.span.start as usize, b.span.end as usize);
        Self::new(
            Kind::Binary(op, Box::new(a), Box::new(b)),
            file,
            start,
            end,
        )
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
            Kind::Char(c) => write!(f, "{}", quote(&c.to_string(), '\''))?,
            Kind::String(s) => write!(f, "{}", quote(s, '"'))?,
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
                write!(f, "let {{ {n} = ")?;
                a.print(f, 0)?;
                write!(f, "; }} in ")?;
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

/// One top-level equation, with parameters already expanded into lambdas.
#[derive(Clone, Debug)]
pub struct Binding {
    pub name: String,
    pub rhs: Expr,
    pub span: Span,
}

/// A single source file with declarations in source order.
#[derive(Clone, Debug)]
pub struct Program {
    pub bindings: Vec<Binding>,
    pub span: Span,
}

impl Program {
    /// Preserve source order: each declaration scopes over only itself and later declarations.
    pub fn expression(&self) -> Expr {
        let mut body = Expr {
            kind: Kind::Var("main".into()),
            span: self.span,
        };
        for binding in self.bindings.iter().rev() {
            body = Expr {
                kind: Kind::Let(
                    binding.name.clone(),
                    Box::new(binding.rhs.clone()),
                    Box::new(body),
                ),
                span: binding.span,
            };
        }
        body
    }
}

/// Haskell-style quoted characters and primitive UTF-8 strings.
pub fn quote(text: &str, delimiter: char) -> String {
    let mut result = String::from(delimiter);
    for c in text.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\t' => result.push_str("\\t"),
            '\0' => result.push_str("\\0"),
            '\\' => result.push_str("\\\\"),
            c if c == delimiter => {
                result.push('\\');
                result.push(c);
            }
            c if c.is_control() => {
                result.push_str(&format!("\\u{{{:x}}}", c as u32))
            }
            c => result.push(c),
        }
    }
    result.push(delimiter);
    result
}

/// Free names account for lexical shadowing and recursive local binding scope.
pub fn free(expr: &Expr) -> std::collections::BTreeSet<String> {
    use std::collections::BTreeSet;
    match &expr.kind {
        Kind::Var(n) => BTreeSet::from([n.clone()]),
        Kind::Lam(n, a) => {
            let mut names = free(a);
            names.remove(n);
            names
        }
        Kind::Let(n, a, b) => {
            let mut names = free(a);
            names.extend(free(b));
            names.remove(n);
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
