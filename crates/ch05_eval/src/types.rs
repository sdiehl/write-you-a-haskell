//! Syntax-directed checking: the environment records one simple type per name.
use crate::{
    ast::{Expr, Kind, Op},
    diagnostic::Diagnostic,
};
use std::fmt;

// ANCHOR: types
/// Simple types have no variables, schemes, or implicit quantification.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Arrow(Box<Type>, Box<Type>),
}
impl Type {
    pub fn arrow(domain: Self, codomain: Self) -> Self {
        Self::Arrow(Box::new(domain), Box::new(codomain))
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Bool => write!(f, "Bool"),
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
// ANCHOR_END: types
fn mismatch(
    expr: &Expr,
    expected: impl fmt::Display,
    found: &Type,
) -> Diagnostic {
    Diagnostic::new(
        "E0201",
        format!("expected {expected}, found {found}"),
        expr.span.clone(),
    )
}
fn expect(
    expr: &Expr,
    found: Type,
    expected: &Type,
) -> Result<(), Diagnostic> {
    if found == *expected {
        Ok(())
    } else {
        Err(mismatch(expr, expected, &found))
    }
}
/// Check a closed term, reporting the source span of the first type error.
pub fn check(expr: &Expr) -> Result<Type, Diagnostic> {
    infer(expr, &mut vec![])
}
// ANCHOR: infer
fn infer(
    expr: &Expr,
    env: &mut Vec<(String, Type)>,
) -> Result<Type, Diagnostic> {
    match &expr.kind {
        Kind::Int(_) => Ok(Type::Int),
        Kind::Bool(_) => Ok(Type::Bool),
        Kind::Var(name) => env
            .iter()
            .rev()
            .find(|(n, _)| n == name)
            .map(|(_, ty)| ty.clone())
            .ok_or_else(|| {
                Diagnostic::new(
                    "E0101",
                    format!("unbound variable {name}"),
                    expr.span.clone(),
                )
            }),
        Kind::Lam(name, parameter, body) => {
            env.push((name.clone(), parameter.clone()));
            let result = infer(body, env);
            env.pop();
            Ok(Type::arrow(parameter.clone(), result?))
        }
        Kind::App(function, argument) => {
            let function_ty = infer(function, env)?;
            let Type::Arrow(domain, codomain) = function_ty else {
                return Err(mismatch(
                    function,
                    "a function type",
                    &function_ty,
                ));
            };
            expect(argument, infer(argument, env)?, &domain)?;
            Ok(*codomain)
        }
        Kind::Let(name, value, body) => {
            let ty = infer(value, env)?;
            env.push((name.clone(), ty));
            let result = infer(body, env);
            env.pop();
            result
        }
        Kind::If(condition, yes, no) => {
            expect(condition, infer(condition, env)?, &Type::Bool)?;
            let ty = infer(yes, env)?;
            expect(no, infer(no, env)?, &ty)?;
            Ok(ty)
        }
        Kind::Neg(value) => {
            expect(value, infer(value, env)?, &Type::Int)?;
            Ok(Type::Int)
        }
        Kind::Binary(op, left, right) => {
            expect(left, infer(left, env)?, &Type::Int)?;
            expect(right, infer(right, env)?, &Type::Int)?;
            Ok(match op {
                Op::Eq | Op::Lt | Op::Le => Type::Bool,
                _ => Type::Int,
            })
        }
        Kind::Fix(function) => {
            let found = infer(function, env)?;
            if let Type::Arrow(domain, codomain) = &found
                && domain == codomain
            {
                return Ok(*domain.clone());
            }
            Err(mismatch(function, "a type of the form T -> T", &found))
        }
        Kind::Error => Err(Diagnostic::new(
            "E0002",
            "unrecovered syntax error",
            expr.span.clone(),
        )),
    }
}
// ANCHOR_END: infer
