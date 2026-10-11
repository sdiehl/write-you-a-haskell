//! Immutable types shared at the boundary of two independent inferencers.
use crate::{
    diagnostics::{Diagnostic, ErrorCode},
    renamed::Expr,
};
use std::{collections::BTreeMap, fmt};
// ANCHOR: types
/// Variables are fresh identities; algorithms choose their own representation of solutions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Char,
    String,
    Var(u32),
    Con(crate::data::TyConId, Vec<Type>),
    Skolem(u32),
    Arrow(Box<Type>, Box<Type>),
}
/// Quantified variables are freshened independently at every occurrence.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scheme {
    pub variables: Vec<u32>,
    pub ty: Type,
}
impl Scheme {
    pub fn mono(ty: Type) -> Self {
        Self {
            variables: vec![],
            ty,
        }
    }
}
impl Type {
    pub fn arrow(a: Self, b: Self) -> Self {
        Self::Arrow(Box::new(a), Box::new(b))
    }
    /// Rename by first occurrence, making principal types comparable up to alpha-renaming.
    pub fn canonical(&self) -> Self {
        fn go(ty: &Type, names: &mut BTreeMap<u32, u32>) -> Type {
            match ty {
                Type::Var(n) => {
                    let next = names.len() as u32;
                    Type::Var(*names.entry(*n).or_insert(next))
                }
                Type::Con(id, args) => Type::Con(
                    id.clone(),
                    args.iter().map(|t| go(t, names)).collect(),
                ),
                Type::Arrow(a, b) => {
                    Type::arrow(go(a, names), go(b, names))
                }
                other => other.clone(),
            }
        }
        go(self, &mut BTreeMap::new())
    }
}
// ANCHOR_END: types
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Bool => write!(f, "Bool"),
            Self::Char => write!(f, "Char"),
            Self::String => write!(f, "String"),
            Self::Con(id, args) => {
                if id.name == "[]" {
                    return write!(f, "[{}]", args[0]);
                }
                if matches!(id.name.as_str(), "(,)" | "(,,)") {
                    write!(f, "(")?;
                    for (i, t) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ",")?;
                        }
                        write!(f, "{t}")?;
                    }
                    return write!(f, ")");
                }
                write!(f, "{}", id.name)?;
                for t in args {
                    if matches!(t, Self::Arrow(..))
                        || matches!(t,Self::Con(_,v) if !v.is_empty())
                    {
                        write!(f, " ({t})")?;
                    } else {
                        write!(f, " {t}")?;
                    }
                }
                Ok(())
            }
            Self::Skolem(n) => write!(f, "s{n}"),
            Self::Var(n) if *n < 26 => {
                write!(f, "{}", char::from(b'a' + *n as u8))
            }
            Self::Var(n) => write!(f, "t{n}"),
            Self::Arrow(a, b) => {
                if matches!(**a, Self::Arrow(..)) {
                    write!(f, "({a})")?;
                } else {
                    write!(f, "{a}")?;
                }
                write!(f, " -> {b}")
            }
        }
    }
}
pub(crate) fn mismatch(expr: &Expr, a: &Type, b: &Type) -> Diagnostic {
    Diagnostic::error(
        ErrorCode::Mismatch,
        format!("cannot unify {a} with {b}"),
        expr.span,
    )
}
pub(crate) fn occurs(expr: &Expr) -> Diagnostic {
    Diagnostic::error(
        ErrorCode::Occurs,
        "occurs check: a type would contain itself",
        expr.span,
    )
}
