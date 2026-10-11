//! Reference execution of strict lambda Core, with explicit lexical closures.
use crate::{
    ast::Op,
    core::{self, Bind, BindGroup, Expr, Id, Lit},
};
use std::{cell::RefCell, rc::Rc};
/// Language termination is separate from an ordinary runtime failure.
#[derive(Debug, Clone)]
pub enum Failure {
    Runtime(String),
    Exit(i32),
}
impl From<String> for Failure {
    fn from(s: String) -> Self {
        Self::Runtime(s)
    }
}
impl From<&str> for Failure {
    fn from(s: &str) -> Self {
        Self::Runtime(s.into())
    }
}
impl std::fmt::Display for Failure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Runtime(s) => write!(f, "{s}"),
            Self::Exit(n) => write!(f, "exit {n}"),
        }
    }
}
#[derive(Debug)]
enum Transfer {
    Failure(Failure),
}
impl From<Failure> for Transfer {
    fn from(e: Failure) -> Self {
        Self::Failure(e)
    }
}
impl From<String> for Transfer {
    fn from(s: String) -> Self {
        Self::Failure(s.into())
    }
}
impl From<&str> for Transfer {
    fn from(s: &str) -> Self {
        Self::Failure(s.into())
    }
}
impl Transfer {
    fn failure(self) -> Failure {
        match self {
            Self::Failure(e) => e,
        }
    }
}
#[derive(Clone, Debug, Default)]
pub struct Env(crate::env::Env<u32, Value>);
impl Env {
    fn new() -> Self {
        Self::default()
    }
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    fn get(&self, id: &Id) -> Option<&Value> {
        self.0.get(&id.unique())
    }
    fn push(&mut self, (id, value): (Id, Value)) {
        self.0.insert(id.unique(), value);
    }
    fn extend(&mut self, entries: impl IntoIterator<Item = (Id, Value)>) {
        for entry in entries {
            self.push(entry);
        }
    }
    fn drain_unique(&mut self, values: &mut Vec<Value>) {
        self.0.drain_unique(values);
    }
}
#[derive(Clone, Debug)]
/// A closure captures a shared outer scope and an optional machine-owned recursive group.
pub enum Value {
    Lit(Lit),
    Con(crate::data::ConId, Rc<Vec<Value>>, Option<Vec<String>>),
    Closure {
        parameter: Id,
        body: usize,
        env: Rc<Env>,
        recursive: Option<usize>,
    },
}
// Releasing a long list or a captured environment must not recurse through Rc.
impl Drop for Value {
    fn drop(&mut self) {
        fn children(value: &mut Value, pending: &mut Vec<Value>) {
            match value {
                Value::Con(_, fields, _) => {
                    if !fields.is_empty()
                        && let Ok(values) = Rc::try_unwrap(
                            std::mem::replace(fields, Rc::new(vec![])),
                        )
                    {
                        pending.extend(values);
                    }
                }
                Value::Closure { env, .. } => {
                    if !env.is_empty()
                        && let Ok(mut values) = Rc::try_unwrap(
                            std::mem::replace(env, Rc::new(Env::new())),
                        )
                    {
                        values.drain_unique(pending);
                    }
                }
                _ => {}
            }
        }
        let mut pending = vec![];
        children(self, &mut pending);
        while let Some(mut value) = pending.pop() {
            children(&mut value, &mut pending);
        }
    }
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
            Self::Lit(Lit::Double(n))
                if f64::from_bits(*n).is_sign_negative()
                    && precedence > 6 =>
            {
                write!(
                    f,
                    "({})",
                    crate::primitives::show_double(f64::from_bits(*n))
                )
            }
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
                    labels.iter().zip(fields.iter()).enumerate()
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
                    for v in fields.iter() {
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
/// Continuations make source calls independent of the Rust stack. Code is
/// borrowed from the immutable program and closures store stable code indices.
struct Machine<'a> {
    groups: Vec<Env>,
    program: &'a core::Program,
    globals: RefCell<Env>,
    code: Vec<&'a Expr>,
    indices: std::collections::BTreeMap<usize, usize>,
    fuel: usize,
}
enum Frame<'a> {
    Function(&'a Expr, Env),
    Argument(Value),
    Case(&'a Id, &'a [core::Alt], Env),
    Join(&'a crate::names::Id, &'a [Id], &'a Expr, Env),
    Args(&'a Expr, &'a [Expr], usize, Vec<Value>, Env),
    Bind(&'a [Bind], usize, Env, Next<'a>),
    Guard(&'a core::Match, usize, usize, Env, Env),
}
#[derive(Clone)]
enum Next<'a> {
    Expr(&'a Expr),
    Row(&'a core::Match, usize, Env),
    Locals(&'a core::Match, usize, usize, Env),
}
enum State<'a> {
    Eval(&'a Expr, Env),
    Return(Value),
    Jump(crate::names::Id, Vec<Value>),
    Next(Next<'a>, Env),
    Row(&'a core::Match, usize, Env),
    Guard(&'a core::Match, usize, usize, Env, Env),
}
impl<'a> Machine<'a> {
    fn code(&mut self, expr: &'a Expr) -> usize {
        let key = expr as *const Expr as usize;
        *self.indices.entry(key).or_insert_with(|| {
            self.code.push(expr);
            self.code.len() - 1
        })
    }
    // ANCHOR: recursive-closures
    fn recursive_values(&mut self, binds: &'a [Bind], env: &Env) -> Env {
        let index = self.groups.len();
        let captured = Rc::new(env.clone());
        let mut group = env.clone();
        for binding in binds {
            let mut rhs = binding.rhs.as_ref();
            while let Expr::TyLam(_, inner) = rhs {
                rhs = inner;
            }
            let Expr::Lam(parameter, body) = rhs else {
                unreachable!("recursive lambda invariant")
            };
            group.push((
                binding.name.clone(),
                Value::Closure {
                    parameter: parameter.clone(),
                    body: self.code(body),
                    env: captured.clone(),
                    recursive: Some(index),
                },
            ));
        }
        // Closures refer to this table by index, so the shared group has no Rc cycle.
        self.groups.push(group.clone());
        group
    }
    // ANCHOR_END: recursive-closures
    fn group(
        &mut self,
        group: &'a BindGroup,
        mut env: Env,
        next: Next<'a>,
        stack: &mut Vec<Frame<'a>>,
    ) -> State<'a> {
        if group.recursive {
            env = self.recursive_values(&group.binds, &env);
        } else if let Some(first) = group.binds.first() {
            stack.push(Frame::Bind(&group.binds, 0, env.clone(), next));
            return State::Eval(&first.rhs, env);
        }
        State::Next(next, env)
    }
    fn arguments(
        &mut self,
        owner: &'a Expr,
        values: Vec<Value>,
    ) -> Result<State<'a>, Transfer> {
        Ok(match owner {
            Expr::Con(id, _, _) => State::Return(Value::Con(
                id.clone(),
                Rc::new(values),
                self.program.constructors[id.index as usize].labels.clone(),
            )),
            Expr::Prim(op, _, _, _) => State::Return(primitive(
                *op,
                &values,
                self.program,
                &self.globals,
            )?),
            Expr::Jump(id, _, _) => State::Jump(id.clone(), values),
            _ => unreachable!(),
        })
    }
    fn evaluate(&mut self, expr: &'a Expr) -> Result<Value, Transfer> {
        self.run(State::Eval(expr, Env::new()))
    }
    fn run(&mut self, mut state: State<'a>) -> Result<Value, Transfer> {
        let mut stack = vec![];
        loop {
            state = match state {
                State::Eval(expr, mut env) => {
                    self.fuel = self
                        .fuel
                        .checked_sub(1)
                        .ok_or("step limit exceeded")?;
                    match expr {
                        Expr::Lit(l) => {
                            State::Return(Value::Lit(l.clone()))
                        }
                        Expr::Var(id) => {
                            State::Return(env.get(id).cloned().ok_or(
                                "internal error: unbound Core variable",
                            )?)
                        }
                        Expr::Global(id) => State::Return(
                            self.globals.borrow().get(id).cloned().ok_or(
                                "internal error: uninitialized global",
                            )?,
                        ),
                        Expr::TyLam(_, b) | Expr::TyApp(b, _) => {
                            State::Eval(b, env)
                        }
                        Expr::Lam(parameter, body) => {
                            State::Return(Value::Closure {
                                parameter: parameter.clone(),
                                body: self.code(body),
                                env: Rc::new(env),
                                recursive: None,
                            })
                        }
                        Expr::App(f, a) => {
                            stack.push(Frame::Function(a, env.clone()));
                            State::Eval(f, env)
                        }
                        Expr::Let(g, b) => {
                            self.group(g, env, Next::Expr(b), &mut stack)
                        }
                        Expr::Case(scrut, binder, _, alts) => {
                            stack.push(Frame::Case(
                                binder,
                                alts,
                                env.clone(),
                            ));
                            State::Eval(scrut, env)
                        }
                        Expr::Join(id, params, body, rest) => {
                            stack.push(Frame::Join(
                                id,
                                params,
                                body,
                                env.clone(),
                            ));
                            State::Eval(rest, env)
                        }
                        Expr::Con(_, _, args)
                        | Expr::Prim(_, _, args, _)
                        | Expr::Jump(_, args, _) => {
                            if let Some(first) = args.first() {
                                stack.push(Frame::Args(
                                    expr,
                                    args,
                                    0,
                                    vec![],
                                    env.clone(),
                                ));
                                State::Eval(first, env)
                            } else {
                                self.arguments(expr, vec![])?
                            }
                        }
                        Expr::PatternFail(span, _) => {
                            return Err(failure(self.program, *span).into());
                        }
                        Expr::Match(m) => {
                            State::Row(m, 0, std::mem::take(&mut env))
                        }
                    }
                }
                State::Next(next, env) => match next {
                    Next::Expr(e) => State::Eval(e, env),
                    Next::Row(m, row, outer) => {
                        State::Guard(m, row, 0, env, outer)
                    }
                    Next::Locals(m, row, group, outer) => {
                        if let Some(g) = m.rows[row].locals.get(group) {
                            self.group(
                                g,
                                env,
                                Next::Locals(m, row, group + 1, outer),
                                &mut stack,
                            )
                        } else {
                            State::Next(Next::Row(m, row, outer), env)
                        }
                    }
                },
                State::Row(m, row, outer) => {
                    let Some(r) = m.rows.get(row) else {
                        return Err(failure(self.program, m.span).into());
                    };
                    let mut local = outer.clone();
                    let matched =
                        r.pats.iter().zip(&m.scruts).all(|(p, id)| {
                            outer.get(id).is_some_and(|v| {
                                matches_pattern(p, v, &mut local)
                            })
                        });
                    if matched {
                        State::Next(Next::Locals(m, row, 0, outer), local)
                    } else {
                        State::Row(m, row + 1, outer)
                    }
                }
                State::Guard(m, row, guard, local, outer) => {
                    match &m.rows[row].body {
                        core::RowBody::Plain(e) => State::Eval(e, local),
                        core::RowBody::Guarded(gs) => {
                            if let Some((g, _)) = gs.get(guard) {
                                stack.push(Frame::Guard(
                                    m,
                                    row,
                                    guard,
                                    local.clone(),
                                    outer,
                                ));
                                State::Eval(g, local)
                            } else {
                                State::Row(m, row + 1, outer)
                            }
                        }
                    }
                }
                State::Jump(id, args) => {
                    let mut target = None;
                    while let Some(frame) = stack.pop() {
                        if let Frame::Join(n, params, body, mut env) = frame
                            && *n == id
                        {
                            let saved = env.clone();
                            env.extend(params.iter().cloned().zip(args));
                            stack.push(Frame::Join(n, params, body, saved));
                            target = Some(State::Eval(body, env));
                            break;
                        }
                    }
                    target.ok_or("internal error: escaped join")?
                }
                State::Return(value) => match stack.pop() {
                    None => return Ok(value),
                    Some(Frame::Join(..)) => State::Return(value),
                    Some(Frame::Function(arg, env)) => {
                        stack.push(Frame::Argument(value));
                        State::Eval(arg, env)
                    }
                    // ANCHOR: application
                    Some(Frame::Argument(function)) => {
                        let Value::Closure {
                            parameter,
                            body,
                            env,
                            recursive,
                        } = &function
                        else {
                            return Err(
                                "internal error: non-function application"
                                    .into(),
                            );
                        };
                        // Joins cannot escape a lambda. Discard tail-position join scopes.
                        while matches!(stack.last(), Some(Frame::Join(..)))
                        {
                            stack.pop();
                        }
                        let mut local = (**env).clone();
                        if let Some(recipe) = recursive {
                            local = self.groups[*recipe].clone();
                        }
                        local.push((parameter.clone(), value));
                        State::Eval(self.code[*body], local)
                    }
                    // ANCHOR_END: application
                    Some(Frame::Case(binder, alts, mut env)) => {
                        let alt = alts
                            .iter()
                            .find(|alt| match (&alt.con, &value) {
                                (core::AltCon::Default, _) => true,
                                (
                                    core::AltCon::Con(a),
                                    Value::Con(b, _, _),
                                ) => a == b,
                                (
                                    core::AltCon::Lit(Lit::Double(a)),
                                    Value::Lit(Lit::Double(b)),
                                ) => {
                                    f64::from_bits(*a) == f64::from_bits(*b)
                                }
                                (core::AltCon::Lit(a), Value::Lit(b)) => {
                                    a == b
                                }
                                _ => false,
                            })
                            .ok_or("internal error: missing alternative")?;
                        if let Value::Con(_, fields, _) = &value {
                            env.extend(
                                alt.binders
                                    .iter()
                                    .cloned()
                                    .zip(fields.iter().cloned()),
                            );
                        }
                        env.push((binder.clone(), value));
                        State::Eval(&alt.rhs, env)
                    }
                    Some(Frame::Args(
                        owner,
                        args,
                        index,
                        mut values,
                        env,
                    )) => {
                        values.push(value);
                        if let Some(next) = args.get(index + 1) {
                            stack.push(Frame::Args(
                                owner,
                                args,
                                index + 1,
                                values,
                                env.clone(),
                            ));
                            State::Eval(next, env)
                        } else {
                            self.arguments(owner, values)?
                        }
                    }
                    Some(Frame::Bind(binds, index, mut env, next)) => {
                        env.push((binds[index].name.clone(), value));
                        if let Some(b) = binds.get(index + 1) {
                            stack.push(Frame::Bind(
                                binds,
                                index + 1,
                                env.clone(),
                                next,
                            ));
                            State::Eval(&b.rhs, env)
                        } else {
                            State::Next(next, env)
                        }
                    }
                    Some(Frame::Guard(m, row, guard, local, outer)) => {
                        if matches!(value, Value::Con(ref id, _, _) if id.name == "True")
                        {
                            let core::RowBody::Guarded(gs) =
                                &m.rows[row].body
                            else {
                                unreachable!()
                            };
                            State::Eval(&gs[guard].1, local)
                        } else {
                            State::Guard(m, row, guard + 1, local, outer)
                        }
                    }
                },
            };
        }
    }
}
fn primitive(
    op: Op,
    args: &[Value],
    program: &core::Program,
    _globals: &RefCell<Env>,
) -> Result<Value, Transfer> {
    if let Op::Runtime(p) = op {
        return crate::primitives::evaluate(p, args, program)
            .map_err(Into::into);
    }
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
        return Ok(Value::Con(
            c.id.clone(),
            Rc::new(vec![]),
            c.labels.clone(),
        ));
    }
    let value = match op {
        Op::Runtime(p) => {
            return crate::primitives::evaluate(p, args, program)
                .map_err(Into::into);
        }
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
fn execute(
    program: &core::Program,
    fuel: usize,
    io: bool,
) -> Result<String, Transfer> {
    let mut machine = Machine {
        groups: vec![],
        program,
        globals: RefCell::new(Env::new()),
        code: vec![],
        indices: Default::default(),
        fuel,
    };
    for functions in [true, false] {
        for group in &program.groups {
            for b in &group.binds {
                if core::is_function(&b.rhs) == functions {
                    let value = machine.evaluate(&b.rhs)?;
                    machine
                        .globals
                        .borrow_mut()
                        .push((b.name.clone(), value));
                }
            }
        }
    }
    let value = machine
        .globals
        .borrow()
        .get(&program.main)
        .cloned()
        .ok_or("internal error: missing main")?;
    if io {
        let unit = program
            .constructors
            .iter()
            .find(|c| c.id.name == "()")
            .ok_or("internal error: missing unit")?;
        let Value::Closure {
            parameter,
            body,
            env,
            recursive,
        } = &value
        else {
            return Err("internal error: main is not IO".into());
        };
        let mut local = (**env).clone();
        if let Some(recipe) = recursive {
            local = machine.groups[*recipe].clone();
        }
        local.push((
            parameter.clone(),
            Value::Con(unit.id.clone(), Rc::new(vec![]), None),
        ));
        machine.run(State::Eval(machine.code[*body], local))?;
        Ok(String::new())
    } else {
        Ok(value.to_string())
    }
}
/// Evaluate a pure fragment; retained for compiler tests and REPL inspection.
pub fn run(program: &core::Program, fuel: usize) -> Result<String, String> {
    execute_checked(program, fuel, false).map_err(|e| e.to_string())
}
/// Initialize globals, then run the checked IO action exactly once.
pub fn run_io(program: &core::Program, fuel: usize) -> Result<(), Failure> {
    execute_checked(program, fuel, true).map(|_| ())
}

fn execute_checked(
    program: &core::Program,
    fuel: usize,
    io: bool,
) -> Result<String, Failure> {
    execute(program, fuel, io).map_err(Transfer::failure)
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
        core::Pat::Lit(Lit::Double(n)) => {
            matches!(v,Value::Lit(Lit::Double(m)) if f64::from_bits(*n)==f64::from_bits(*m))
        }
        core::Pat::Lit(l) => matches!(v,Value::Lit(actual) if l==actual),
        core::Pat::Con(id, ps) => match v {
            Value::Con(actual, vs, _)
                if id == actual && ps.len() == vs.len() =>
            {
                ps.iter()
                    .zip(vs.iter())
                    .all(|(p, v)| matches_pattern(p, v, env))
            }
            _ => false,
        },
    }
}
// ANCHOR_END: match-pattern
