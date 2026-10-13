//! Reference execution of strict lambda Core, with explicit lexical closures.
use crate::{
    ast::Op,
    core::{self, Bind, BindGroup, Expr, Id, Lit},
};
use std::rc::Rc;

type Env = Vec<(Id, Value)>;

#[derive(Clone, Debug)]
/// A closure captures outer declarations and an optional immutable recursive-group recipe.
pub enum Value {
    Lit(Lit),
    Con(crate::data::ConId, Vec<Value>, Option<Vec<String>>),
    Closure {
        parameter: Id,
        body: Rc<Expr>,
        env: Rc<Env>,
        recursive: Option<Rc<Vec<Bind>>>,
    },
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print(f, 0)
    }
}

impl Value {
    fn print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        precedence: u8,
    ) -> std::fmt::Result {
        match self {
            Self::Lit(Lit::Int(n)) if *n < 0 && precedence > 6 => {
                write!(f, "({n})")
            }
            Self::Lit(l) => write!(f, "{l}"),
            Self::Closure { .. } => write!(f, "<function>"),
            Self::Con(id, fields, Some(labels)) => {
                if precedence > 10 {
                    write!(f, "(")?;
                }
                write!(f, "{} {{", id.name)?;
                for (i, (name, value)) in
                    labels.iter().zip(fields).enumerate()
                {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{name} = ")?;
                    value.print(f, 0)?;
                }
                write!(f, "}}")?;
                if precedence > 10 {
                    write!(f, ")")?;
                }
                Ok(())
            }
            Self::Con(id, fields, None) => match id.name.as_str() {
                "[]" => write!(f, "[]"),
                ":" => {
                    write!(f, "[")?;
                    let mut value = self;
                    let mut first = true;
                    while let Self::Con(c, fs, _) = value {
                        if c.name == "[]" {
                            break;
                        }
                        if !first {
                            write!(f, ",")?;
                        }
                        first = false;
                        fs[0].print(f, 0)?;
                        value = &fs[1];
                    }
                    write!(f, "]")
                }
                "()" | "(,)" | "(,,)" => {
                    write!(f, "(")?;
                    for (i, v) in fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ",")?;
                        }
                        v.print(f, 0)?;
                    }
                    write!(f, ")")
                }
                _ => {
                    let parens = precedence > 10 && !fields.is_empty();
                    if parens {
                        write!(f, "(")?;
                    }
                    write!(f, "{}", id.name)?;
                    for v in fields {
                        write!(f, " ")?;
                        v.print(f, 11)?;
                    }
                    if parens {
                        write!(f, ")")?;
                    }
                    Ok(())
                }
            },
        }
    }
}

pub fn normalize(n: i64) -> i64 {
    (n << 1) >> 1
}

/// Rebuild a recursive group's closures from a shared immutable recipe. The
/// recipe captures the outer environment, never the closures themselves.
// ANCHOR: recursive-closures
fn recursive_values(bindings: Rc<Vec<Bind>>, env: Rc<Env>) -> Env {
    let mut values = Vec::new();
    for binding in bindings.iter() {
        let mut rhs = &*binding.rhs;
        while let Expr::TyLam(_, inner) = rhs {
            rhs = inner;
        }
        let Expr::Lam(parameter, body) = rhs else {
            unreachable!("recursive lambda invariant")
        };
        let value = Value::Closure {
            parameter: parameter.clone(),
            body: Rc::new(*body.clone()),
            env: env.clone(),
            recursive: Some(bindings.clone()),
        };
        values.push((binding.name.clone(), value));
    }
    values
}
// ANCHOR_END: recursive-closures

fn bind_group(
    group: &BindGroup,
    env: &mut Env,
    fuel: &mut usize,
    program: &core::Program,
) -> Result<(), String> {
    if group.recursive {
        env.extend(recursive_values(
            Rc::new(group.binds.clone()),
            Rc::new(env.clone()),
        ));
    } else {
        for binding in &group.binds {
            let value = evaluate(&binding.rhs, env, fuel, program)?;
            env.push((binding.name.clone(), value));
        }
    }
    Ok(())
}

fn evaluate(
    expr: &Expr,
    env: &Env,
    fuel: &mut usize,
    program: &core::Program,
) -> Result<Value, String> {
    *fuel = fuel.checked_sub(1).ok_or("step limit exceeded")?;
    match expr {
        Expr::Lit(l) => Ok(Value::Lit(l.clone())),
        Expr::TyLam(_, body) | Expr::TyApp(body, _) => {
            evaluate(body, env, fuel, program)
        }
        Expr::Var(n) | Expr::Global(n) => env
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
            let function = evaluate(function, env, fuel, program)?;
            let argument = evaluate(argument, env, fuel, program)?;
            if let Value::Closure {
                parameter,
                body,
                env,
                recursive,
            } = &function
            {
                let mut local = (**env).clone();
                if let Some(bindings) = recursive {
                    local.extend(recursive_values(
                        bindings.clone(),
                        env.clone(),
                    ));
                }
                local.push((parameter.clone(), argument));
                evaluate(body, &local, fuel, program)
            } else {
                Err("internal error: non-function application".into())
            }
        }
        // ANCHOR_END: application
        Expr::Let(group, body) => {
            let mut local = env.clone();
            bind_group(group, &mut local, fuel, program)?;
            evaluate(body, &local, fuel, program)
        }
        Expr::Con(id, _, fields) => Ok(Value::Con(
            id.clone(),
            fields
                .iter()
                .map(|e| evaluate(e, env, fuel, program))
                .collect::<Result<_, _>>()?,
            program.constructors[id.index as usize].labels.clone(),
        )),
        Expr::PatternFail(span, _) => Err(failure(program, *span)),
        Expr::Match(m) => {
            let values = m
                .scruts
                .iter()
                .map(|id| {
                    evaluate(&Expr::Var(id.clone()), env, fuel, program)
                })
                .collect::<Result<Vec<_>, _>>()?;
            for row in &m.rows {
                let mut local = env.clone();
                if !row
                    .pats
                    .iter()
                    .zip(&values)
                    .all(|(p, v)| matches_pattern(p, v, &mut local))
                {
                    continue;
                }
                for group in &row.locals {
                    bind_group(group, &mut local, fuel, program)?;
                }
                match &row.body {
                    core::RowBody::Plain(e) => {
                        return evaluate(e, &local, fuel, program);
                    }
                    core::RowBody::Guarded(gs) => {
                        for (g, e) in gs {
                            let value = evaluate(g, &local, fuel, program)?;
                            if matches!(value,Value::Con(id,_,_) if id.name=="True")
                            {
                                return evaluate(e, &local, fuel, program);
                            }
                        }
                    }
                }
            }
            Err(failure(program, m.span))
        }
        Expr::Prim(op, _, args, _) => {
            let values = args
                .iter()
                .map(|a| evaluate(a, env, fuel, program))
                .collect::<Result<Vec<_>, _>>()?;
            primitive(*op, &values, program)
        }
    }
}

fn primitive(
    op: Op,
    args: &[Value],
    program: &core::Program,
) -> Result<Value, String> {
    if let [Value::Lit(Lit::String(a)), Value::Lit(Lit::String(b))] = args
        && op == Op::Append
    {
        return Ok(Value::Lit(Lit::String(format!("{a}{b}"))));
    }
    let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = args else {
        return Err("internal error: primitive operands".into());
    };
    if matches!(op, Op::Eq | Op::Lt | Op::Le) {
        let yes = match op {
            Op::Eq => a == b,
            Op::Lt => a < b,
            _ => a <= b,
        };
        let name = if yes { "True" } else { "False" };
        let c = program
            .constructors
            .iter()
            .find(|c| c.id.name == name)
            .ok_or("internal error: missing Bool constructor")?;
        return Ok(Value::Con(c.id.clone(), vec![], c.labels.clone()));
    }
    let value = match op {
        Op::Add => Lit::Int(normalize(a.wrapping_add(*b))),
        Op::Sub => Lit::Int(normalize(a.wrapping_sub(*b))),
        Op::Mul => Lit::Int(normalize(a.wrapping_mul(*b))),
        Op::Eq | Op::Lt | Op::Le => unreachable!(),
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

/// Evaluate top-level values exactly once in dependency order, then select main's value.
pub fn run(
    program: &core::Program,
    mut fuel: usize,
) -> Result<String, String> {
    let mut env = vec![];
    for group in &program.groups {
        bind_group(group, &mut env, &mut fuel, program)?;
    }
    env.iter()
        .rev()
        .find(|(n, _)| n == &program.main)
        .map(|(_, v)| v.to_string())
        .ok_or_else(|| "internal error: missing main".into())
}

fn failure(program: &core::Program, span: crate::source::Span) -> String {
    let site = program
        .locations
        .iter()
        .find(|(s, _)| *s == span)
        .map(|(_, s)| s.as_str())
        .unwrap_or("<unknown>:1:1");
    format!("pattern match failure at {site}")
}

// ANCHOR: match-pattern
fn matches_pattern(p: &core::Pat, v: &Value, env: &mut Env) -> bool {
    match p {
        core::Pat::Wild => true,
        core::Pat::Var(id) => {
            env.push((id.clone(), v.clone()));
            true
        }
        core::Pat::As(id, p) => {
            env.push((id.clone(), v.clone()));
            matches_pattern(p, v, env)
        }
        core::Pat::Lit(l) => matches!(v,Value::Lit(actual) if l==actual),
        core::Pat::Con(id, ps) => match v {
            Value::Con(actual, vs, _)
                if id == actual && ps.len() == vs.len() =>
            {
                ps.iter().zip(vs).all(|(p, v)| matches_pattern(p, v, env))
            }
            _ => false,
        },
    }
}
// ANCHOR_END: match-pattern
