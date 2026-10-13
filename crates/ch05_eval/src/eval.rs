//! Big-step call-by-value evaluation with lexical closures and bounded fuel.
use crate::{
    ast::{Expr, Kind, Op},
    diagnostic::Diagnostic,
};
use std::{fmt, rc::Rc};

// ANCHOR: value
/// Lexical bindings retained by a closure.
pub type Env = Vec<(String, Value)>;

#[derive(Clone, Debug)]
/// Runtime integers, booleans, and code paired with its lexical environment.
pub enum Value {
    Int(i64),
    Bool(bool),
    Closure(String, Rc<Expr>, Rc<Env>),
    /// A recursive reference unfolds only when the variable is read.
    Recursive(Rc<Value>),
}
// ANCHOR_END: value

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{n}"),
            Self::Bool(b) => {
                write!(f, "{}", if *b { "True" } else { "False" })
            }
            Self::Closure(..) | Self::Recursive(..) => {
                write!(f, "<function>")
            }
        }
    }
}

pub fn normalize(n: i64) -> i64 {
    (n << 1) >> 1
}

pub fn primitive(op: Op, a: i64, b: i64) -> Result<Value, String> {
    let (a, b) = (normalize(a), normalize(b));
    Ok(match op {
        Op::Eq => Value::Bool(a == b),
        Op::Lt => Value::Bool(a < b),
        Op::Le => Value::Bool(a <= b),
        Op::Add => Value::Int(normalize(a.wrapping_add(b))),
        Op::Sub => Value::Int(normalize(a.wrapping_sub(b))),
        Op::Mul => Value::Int(normalize(a.wrapping_mul(b))),
        Op::Div if b == 0 => return Err("divide by zero".into()),
        Op::Div => Value::Int(normalize(a / b)),
    })
}

pub(crate) fn integer(value: Value) -> Result<i64, String> {
    if let Value::Int(n) = value {
        Ok(n)
    } else {
        Err("expected Int".into())
    }
}

/// Each visit consumes fuel, including visits that precede a recursive call.
pub fn evaluate(
    expr: &Expr,
    env: &Env,
    fuel: &mut usize,
) -> Result<Value, String> {
    *fuel = fuel.checked_sub(1).ok_or("step limit exceeded")?;
    match &expr.kind {
        Kind::Int(n) => Ok(Value::Int(normalize(*n))),
        Kind::Bool(b) => Ok(Value::Bool(*b)),
        Kind::Var(n) => env
            .iter()
            .rev()
            .find(|(key, _)| key == n)
            .map(|(_, value)| value.clone())
            .ok_or_else(|| format!("unbound variable `{n}`"))
            .and_then(|value| match value {
                Value::Recursive(f) => {
                    apply((*f).clone(), Value::Recursive(f), fuel)
                }
                other => Ok(other),
            }),
        Kind::Lam(n, _, body) => Ok(Value::Closure(
            n.clone(),
            Rc::new(*body.clone()),
            Rc::new(env.clone()),
        )),
        // ANCHOR: cbv-application
        Kind::App(function, argument) => {
            let function = evaluate(function, env, fuel)?;
            let argument = evaluate(argument, env, fuel)?;
            apply(function, argument, fuel)
        }
        // ANCHOR_END: cbv-application
        Kind::Let(n, a, b) => {
            let value = evaluate(a, env, fuel)?;
            let mut local = env.clone();
            local.push((n.clone(), value));
            evaluate(b, &local, fuel)
        }
        Kind::If(c, a, b) => match evaluate(c, env, fuel)? {
            Value::Bool(true) => evaluate(a, env, fuel),
            Value::Bool(false) => evaluate(b, env, fuel),
            _ => Err("expected Bool".into()),
        },
        Kind::Neg(a) => Ok(Value::Int(normalize(
            integer(evaluate(a, env, fuel)?)?.wrapping_neg(),
        ))),
        Kind::Binary(op, a, b) => {
            let left = integer(evaluate(a, env, fuel)?)?;
            let right = integer(evaluate(b, env, fuel)?)?;
            primitive(*op, left, right)
        }
        Kind::Fix(f) => {
            let function = evaluate(f, env, fuel)?;
            let recursive = Value::Recursive(Rc::new(function.clone()));
            apply(function, recursive, fuel)
        }
        Kind::Error => Err("internal error: recovery node".into()),
    }
}

/// Collect unbound names before typing or evaluation.
pub fn check(expr: &Expr) -> Result<(), Vec<Diagnostic>> {
    fn visit(
        expr: &Expr,
        names: &mut Vec<String>,
        errors: &mut Vec<Diagnostic>,
    ) {
        match &expr.kind {
            Kind::Var(n) if !names.contains(n) => {
                errors.push(Diagnostic::new(
                    "E0101",
                    format!("unbound variable `{n}`"),
                    expr.span.clone(),
                ))
            }
            Kind::Lam(n, _, a) => {
                names.push(n.clone());
                visit(a, names, errors);
                names.pop();
            }
            Kind::Let(n, a, b) => {
                visit(a, names, errors);
                names.push(n.clone());
                visit(b, names, errors);
                names.pop();
            }
            Kind::App(a, b) | Kind::Binary(_, a, b) => {
                visit(a, names, errors);
                visit(b, names, errors);
            }
            Kind::If(c, a, b) => {
                visit(c, names, errors);
                visit(a, names, errors);
                visit(b, names, errors);
            }
            Kind::Neg(a) | Kind::Fix(a) => visit(a, names, errors),
            _ => {}
        }
    }
    let mut errors = Vec::new();
    visit(expr, &mut vec![], &mut errors);
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Enter a closure using its definition environment, never the caller's scope.
pub fn apply(
    function: Value,
    argument: Value,
    fuel: &mut usize,
) -> Result<Value, String> {
    if let Value::Closure(name, body, captured) = function {
        let mut local = (*captured).clone();
        local.push((name, argument));
        evaluate(&body, &local, fuel)
    } else {
        Err("expected a function".into())
    }
}
