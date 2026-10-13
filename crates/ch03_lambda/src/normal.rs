//! Leftmost-outermost small steps, including reduction under lambdas.
use crate::{
    ast::{Expr, Kind},
    eval::{self, Value},
    names::substitute,
};

fn rewrap(original: &Expr, kind: Kind) -> Expr {
    Expr::new(kind, original.span.start, original.span.end)
}

/// Perform at most one reduction; `None` means a normal form, possibly open.
pub fn step(expr: &Expr) -> Result<Option<Expr>, String> {
    let kind = match &expr.kind {
        // ANCHOR: beta
        Kind::App(function, argument) => {
            if let Kind::Lam(name, body) = &function.kind {
                return Ok(Some(substitute(body, name, argument)));
            }
            if let Some(next) = step(function)? {
                Kind::App(Box::new(next), argument.clone())
            } else if let Some(next) = step(argument)? {
                Kind::App(function.clone(), Box::new(next))
            } else {
                return Ok(None);
            }
        }
        // ANCHOR_END: beta
        Kind::Lam(n, body) => match step(body)? {
            Some(next) => Kind::Lam(n.clone(), Box::new(next)),
            None => return Ok(None),
        },
        Kind::Let(n, a, b) => return Ok(Some(substitute(b, n, a))),
        Kind::If(c, a, b) => match &c.kind {
            Kind::Bool(true) => return Ok(Some(*a.clone())),
            Kind::Bool(false) => return Ok(Some(*b.clone())),
            _ => match step(c)? {
                Some(next) => {
                    Kind::If(Box::new(next), a.clone(), b.clone())
                }
                None if matches!(c.kind, Kind::Int(_) | Kind::Lam(..)) => {
                    return Err("expected Bool".into());
                }
                None => return Ok(None),
            },
        },
        Kind::Neg(a) => match a.kind {
            Kind::Int(n) => Kind::Int(eval::normalize(n.wrapping_neg())),
            _ => match step(a)? {
                Some(next) => Kind::Neg(Box::new(next)),
                None if matches!(a.kind, Kind::Bool(_) | Kind::Lam(..)) => {
                    return Err("expected Int".into());
                }
                None => return Ok(None),
            },
        },
        Kind::Binary(op, a, b) => {
            if let (Kind::Int(a), Kind::Int(b)) = (&a.kind, &b.kind) {
                match eval::primitive(*op, *a, *b)? {
                    Value::Int(n) => Kind::Int(n),
                    Value::Bool(b) => Kind::Bool(b),
                    _ => unreachable!(),
                }
            } else if let Some(next) = step(a)? {
                Kind::Binary(*op, Box::new(next), b.clone())
            } else if let Some(next) = step(b)? {
                Kind::Binary(*op, a.clone(), Box::new(next))
            } else {
                if matches!(a.kind, Kind::Bool(_) | Kind::Lam(..))
                    || matches!(b.kind, Kind::Bool(_) | Kind::Lam(..))
                {
                    return Err("expected Int".into());
                }
                return Ok(None);
            }
        }
        Kind::Int(n) if *n != eval::normalize(*n) => {
            Kind::Int(eval::normalize(*n))
        }
        Kind::Error => return Err("internal error: recovery node".into()),
        _ => return Ok(None),
    };
    Ok(Some(rewrap(expr, kind)))
}

/// Reduce with a trace callback; bounded fuel also makes divergent examples inspectable.
pub fn reduce(
    mut expr: Expr,
    limit: usize,
    mut trace: impl FnMut(&Expr),
) -> Result<Expr, String> {
    trace(&expr);
    for _ in 0..limit {
        match step(&expr)? {
            Some(next) => {
                expr = next;
                trace(&expr);
            }
            None => return Ok(expr),
        }
    }
    if step(&expr)?.is_none() {
        Ok(expr)
    } else {
        Err("step limit exceeded".into())
    }
}
