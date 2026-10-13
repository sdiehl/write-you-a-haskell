//! A call-by-value machine: Rust recursion is replaced by data and one loop.
use crate::{
    ast::{Expr, Kind, Op},
    eval::{self, Env, Value},
};
use std::rc::Rc;

// ANCHOR: machine
/// The remaining work after the current expression returns a value.
enum Frame {
    Argument(Rc<Expr>, Rc<Env>),
    Apply(Value),
    Let(String, Rc<Expr>, Rc<Env>),
    If(Rc<Expr>, Rc<Expr>, Rc<Env>),
    Neg,
    Left(Op, Rc<Expr>, Rc<Env>),
    Right(Op, i64),
    Fix,
}

/// Control alternates between evaluating an expression and returning a value.
enum Control {
    Expr(Rc<Expr>, Rc<Env>),
    Value(Value),
    Apply(Value, Value),
}

/// Statistics make tail-space behavior testable without relying on timing.
pub struct ResultValue {
    pub value: Value,
    pub steps: usize,
    pub max_continuations: usize,
}
// ANCHOR_END: machine

fn expression(expr: &Expr, env: &Rc<Env>) -> Control {
    Control::Expr(Rc::new(expr.clone()), env.clone())
}

/// Run with bounded transitions and constant Rust stack, including tail calls.
pub fn evaluate(expr: &Expr, fuel: usize) -> Result<ResultValue, String> {
    let mut control = expression(expr, &Rc::new(vec![]));
    let mut stack = Vec::new();
    let mut max_continuations = 0;
    for steps in 1..=fuel {
        max_continuations = max_continuations.max(stack.len());
        control = match control {
            Control::Expr(expr, env) => match &expr.kind {
                Kind::Int(n) => {
                    Control::Value(Value::Int(eval::normalize(*n)))
                }
                Kind::Bool(b) => Control::Value(Value::Bool(*b)),
                Kind::Var(n) => {
                    let value = env
                        .iter()
                        .rev()
                        .find(|(name, _)| name == n)
                        .map(|(_, v)| v.clone())
                        .ok_or_else(|| format!("unbound variable `{n}`"))?;
                    match value {
                        Value::Recursive(f) => Control::Apply(
                            (*f).clone(),
                            Value::Recursive(f),
                        ),
                        other => Control::Value(other),
                    }
                }
                Kind::Lam(n, _, body) => Control::Value(Value::Closure(
                    n.clone(),
                    Rc::new(*body.clone()),
                    env,
                )),
                Kind::App(f, a) => {
                    stack.push(Frame::Argument(
                        Rc::new(*a.clone()),
                        env.clone(),
                    ));
                    expression(f, &env)
                }
                Kind::Let(n, a, b) => {
                    stack.push(Frame::Let(
                        n.clone(),
                        Rc::new(*b.clone()),
                        env.clone(),
                    ));
                    expression(a, &env)
                }
                Kind::If(c, a, b) => {
                    stack.push(Frame::If(
                        Rc::new(*a.clone()),
                        Rc::new(*b.clone()),
                        env.clone(),
                    ));
                    expression(c, &env)
                }
                Kind::Neg(a) => {
                    stack.push(Frame::Neg);
                    expression(a, &env)
                }
                Kind::Binary(op, a, b) => {
                    stack.push(Frame::Left(
                        *op,
                        Rc::new(*b.clone()),
                        env.clone(),
                    ));
                    expression(a, &env)
                }
                Kind::Fix(f) => {
                    stack.push(Frame::Fix);
                    expression(f, &env)
                }
                Kind::Error => {
                    return Err("internal error: recovery node".into());
                }
            },
            // ANCHOR: tail-transfer
            Control::Apply(
                Value::Closure(name, body, captured),
                argument,
            ) => {
                let mut env = (*captured).clone();
                env.push((name, argument));
                Control::Expr(body, Rc::new(env))
            }
            Control::Apply(_, _) => {
                return Err("expected a function".into());
            }
            // ANCHOR_END: tail-transfer
            Control::Value(value) => match stack.pop() {
                None => {
                    return Ok(ResultValue {
                        value,
                        steps,
                        max_continuations,
                    });
                }
                Some(Frame::Argument(argument, env)) => {
                    stack.push(Frame::Apply(value));
                    Control::Expr(argument, env)
                }
                Some(Frame::Apply(function)) => {
                    Control::Apply(function, value)
                }
                Some(Frame::Let(name, body, env)) => {
                    let mut env = (*env).clone();
                    env.push((name, value));
                    Control::Expr(body, Rc::new(env))
                }
                Some(Frame::If(yes, no, env)) => match value {
                    Value::Bool(b) => {
                        Control::Expr(if b { yes } else { no }, env)
                    }
                    _ => return Err("expected Bool".into()),
                },
                Some(Frame::Neg) => Control::Value(Value::Int(
                    eval::normalize(eval::integer(value)?.wrapping_neg()),
                )),
                Some(Frame::Left(op, right, env)) => {
                    stack.push(Frame::Right(op, eval::integer(value)?));
                    Control::Expr(right, env)
                }
                Some(Frame::Right(op, left)) => Control::Value(
                    eval::primitive(op, left, eval::integer(value)?)?,
                ),
                Some(Frame::Fix) => {
                    let recursive =
                        Value::Recursive(Rc::new(value.clone()));
                    Control::Apply(value, recursive)
                }
            },
        };
    }
    Err("step limit exceeded".into())
}
