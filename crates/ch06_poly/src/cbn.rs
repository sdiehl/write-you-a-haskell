//! A substitution-based call-by-name interpreter for strategy comparisons only.
use crate::{
    ast::{Expr, Kind},
    eval::{self, Value},
    names::substitute,
};

/// Evaluate only as far as a literal or lambda; arguments and lets are substituted unevaluated.
pub fn evaluate(expr: &Expr, fuel: &mut usize) -> Result<Expr, String> {
    *fuel = fuel.checked_sub(1).ok_or("step limit exceeded")?;
    let wrap = |kind| Expr::new(kind, expr.span.start, expr.span.end);
    match &expr.kind {
        Kind::Int(n) => Ok(wrap(Kind::Int(eval::normalize(*n)))),
        Kind::Bool(_) | Kind::Lam(..) => Ok(expr.clone()),
        // ANCHOR: call-by-name
        Kind::App(f, a) => {
            let function = evaluate(f, fuel)?;
            if let Kind::Lam(name, body) = function.kind {
                evaluate(&substitute(&body, &name, a), fuel)
            } else {
                Err("expected a function".into())
            }
        }
        Kind::Let(n, a, b) => evaluate(&substitute(b, n, a), fuel),
        // ANCHOR_END: call-by-name
        Kind::If(c, a, b) => match evaluate(c, fuel)?.kind {
            Kind::Bool(true) => evaluate(a, fuel),
            Kind::Bool(false) => evaluate(b, fuel),
            _ => Err("expected Bool".into()),
        },
        Kind::Neg(a) => match evaluate(a, fuel)?.kind {
            Kind::Int(n) => {
                Ok(wrap(Kind::Int(eval::normalize(n.wrapping_neg()))))
            }
            _ => Err("expected Int".into()),
        },
        Kind::Binary(op, a, b) => {
            let a = evaluate(a, fuel)?;
            let b = evaluate(b, fuel)?;
            if let (Kind::Int(a), Kind::Int(b)) = (a.kind, b.kind) {
                match eval::primitive(*op, a, b)? {
                    Value::Int(n) => Ok(wrap(Kind::Int(n))),
                    Value::Bool(b) => Ok(wrap(Kind::Bool(b))),
                    _ => unreachable!(),
                }
            } else {
                Err("expected Int".into())
            }
        }
        Kind::Fix(f) => evaluate(
            &wrap(Kind::App(f.clone(), Box::new(expr.clone()))),
            fuel,
        ),
        Kind::Var(n) => Err(format!("unbound variable `{n}`")),
        Kind::Error => Err("internal error: recovery node".into()),
    }
}
