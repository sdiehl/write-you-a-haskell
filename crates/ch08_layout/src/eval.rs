//! Reference execution of strict lambda Core, with explicit lexical closures.
use crate::{
    ast::Op,
    core::{self, Bind, Expr, Lit},
};
use std::rc::Rc;

type Env = Vec<(String, Value)>;

#[derive(Clone, Debug)]
/// A function captures prior declarations; self is added only when entering a recursive closure.
pub enum Value {
    Lit(Lit),
    Closure {
        parameter: String,
        body: Rc<Expr>,
        env: Rc<Env>,
        recursive: Option<String>,
    },
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lit(l) => write!(f, "{l}"),
            Self::Closure { .. } => write!(f, "<function>"),
        }
    }
}

pub fn normalize(n: i64) -> i64 {
    (n << 1) >> 1
}

fn bind(
    binding: &Bind,
    env: &Env,
    fuel: &mut usize,
) -> Result<Value, String> {
    let mut value = evaluate(&binding.rhs, env, fuel)?;
    if binding.recursive {
        if let Value::Closure { recursive, .. } = &mut value {
            *recursive = Some(binding.name.clone());
        } else {
            return Err("internal error: recursive value".into());
        }
    }
    Ok(value)
}

fn evaluate(
    expr: &Expr,
    env: &Env,
    fuel: &mut usize,
) -> Result<Value, String> {
    *fuel = fuel.checked_sub(1).ok_or("step limit exceeded")?;
    match expr {
        Expr::Lit(l) => Ok(Value::Lit(l.clone())),
        Expr::Var(n) => env
            .iter()
            .rev()
            .find(|(name, _)| name == n)
            .map(|(_, v)| v.clone())
            .ok_or_else(|| format!("internal error: unbound {n}")),
        Expr::Lam(parameter, body) => Ok(Value::Closure {
            parameter: parameter.clone(),
            body: Rc::new(*body.clone()),
            env: Rc::new(env.clone()),
            recursive: None,
        }),
        // ANCHOR: application
        Expr::App(function, argument) => {
            let function = evaluate(function, env, fuel)?;
            let argument = evaluate(argument, env, fuel)?;
            if let Value::Closure {
                parameter,
                body,
                env,
                recursive,
            } = &function
            {
                let mut local = (**env).clone();
                if let Some(name) = recursive {
                    local.push((name.clone(), function.clone()));
                }
                local.push((parameter.clone(), argument));
                evaluate(body, &local, fuel)
            } else {
                Err("internal error: non-function application".into())
            }
        }
        // ANCHOR_END: application
        Expr::Let(binding, body) => {
            let value = bind(binding, env, fuel)?;
            let mut local = env.clone();
            local.push((binding.name.clone(), value));
            evaluate(body, &local, fuel)
        }
        Expr::If(condition, yes, no) => {
            match evaluate(condition, env, fuel)? {
                Value::Lit(Lit::Bool(b)) => {
                    evaluate(if b { yes } else { no }, env, fuel)
                }
                _ => Err("internal error: non-Bool condition".into()),
            }
        }
        Expr::Prim(op, args, _) => {
            let values = args
                .iter()
                .map(|a| evaluate(a, env, fuel))
                .collect::<Result<Vec<_>, _>>()?;
            primitive(*op, &values)
        }
    }
}

fn primitive(op: Op, args: &[Value]) -> Result<Value, String> {
    if let [Value::Lit(Lit::String(a)), Value::Lit(Lit::String(b))] = args
        && op == Op::Append
    {
        return Ok(Value::Lit(Lit::String(format!("{a}{b}"))));
    }
    let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = args else {
        return Err("internal error: primitive operands".into());
    };
    let value = match op {
        Op::Add => Lit::Int(normalize(a.wrapping_add(*b))),
        Op::Sub => Lit::Int(normalize(a.wrapping_sub(*b))),
        Op::Mul => Lit::Int(normalize(a.wrapping_mul(*b))),
        Op::Eq => Lit::Bool(a == b),
        Op::Lt => Lit::Bool(a < b),
        Op::Le => Lit::Bool(a <= b),
        Op::Div | Op::Mod => {
            if *b == 0 {
                return Err("divide by zero".into());
            }
            let (mut q, mut r) = (a / b, a % b);
            if r != 0 && (r < 0) != (*b < 0) {
                q -= 1;
                r += b;
            }
            Lit::Int(normalize(if op == Op::Div { q } else { r }))
        }
        Op::Append => {
            return Err("internal error: string append on Int".into());
        }
    };
    Ok(Value::Lit(value))
}

/// Evaluate top-level values exactly once in source order, then select main's value.
pub fn run(
    program: &core::Program,
    mut fuel: usize,
) -> Result<String, String> {
    let mut env = vec![];
    for binding in &program.bindings {
        let value = bind(binding, &env, &mut fuel)?;
        env.push((binding.name.clone(), value));
    }
    env.iter()
        .rev()
        .find(|(n, _)| n == &program.main)
        .map(|(_, v)| v.to_string())
        .ok_or_else(|| "internal error: missing main".into())
}
