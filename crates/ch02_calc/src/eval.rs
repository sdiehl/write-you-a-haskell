//! Strict evaluation with lexical scope and wrapping signed 63-bit integers.
use crate::{
    ast::{Expr, Kind, Op},
    diagnostic::Diagnostic,
};

pub fn normalize(n: i64) -> i64 {
    (n << 1) >> 1
}

/// Check every variable occurrence without executing arithmetic.
pub fn check(expr: &Expr) -> Result<(), Vec<Diagnostic>> {
    fn visit(
        expr: &Expr,
        names: &mut Vec<String>,
        errors: &mut Vec<Diagnostic>,
    ) {
        match &expr.kind {
            Kind::Var(name) if !names.contains(name) => {
                errors.push(Diagnostic::new(
                    "E0101",
                    format!("unbound variable `{name}`"),
                    expr.span.clone(),
                ))
            }
            Kind::Neg(a) => visit(a, names, errors),
            Kind::Binary(_, a, b) => {
                visit(a, names, errors);
                visit(b, names, errors);
            }
            Kind::Let(name, a, b) => {
                visit(a, names, errors);
                names.push(name.clone());
                visit(b, names, errors);
                names.pop();
            }
            _ => {}
        }
    }
    let mut errors = Vec::new();
    visit(expr, &mut Vec::new(), &mut errors);
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

// ANCHOR: evaluate
/// The environment is a stack: the most recent binding shadows earlier names.
pub fn evaluate(
    expr: &Expr,
    env: &mut Vec<(String, i64)>,
) -> Result<i64, String> {
    match &expr.kind {
        Kind::Int(n) => Ok(normalize(*n)),
        Kind::Var(name) => env
            .iter()
            .rev()
            .find(|(key, _)| key == name)
            .map(|(_, value)| *value)
            .ok_or_else(|| format!("unbound variable `{name}`")),
        Kind::Neg(value) => {
            Ok(normalize(evaluate(value, env)?.wrapping_neg()))
        }
        Kind::Binary(op, left, right) => {
            let a = evaluate(left, env)?;
            let b = evaluate(right, env)?;
            Ok(normalize(match op {
                Op::Add => a.wrapping_add(b),
                Op::Sub => a.wrapping_sub(b),
                Op::Mul => a.wrapping_mul(b),
                Op::Div if b == 0 => return Err("divide by zero".into()),
                Op::Div => a / b,
            }))
        }
        Kind::Let(name, value, body) => {
            let value = evaluate(value, env)?;
            env.push((name.clone(), value));
            let result = evaluate(body, env);
            env.pop();
            result
        }
        Kind::Error => {
            Err("internal error: attempted to evaluate a recovery node"
                .into())
        }
    }
}
// ANCHOR_END: evaluate
