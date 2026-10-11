//! An iterative CEK machine. Calls and returns change state, never the Rust stack.
use crate::{
    anf::{self, Anf, Atom, Body, Lambda, Rhs, Var},
    ast::Op,
    core::{AltCon, Lit},
    data::ConId,
    eval::Failure,
};
use std::{collections::BTreeMap, rc::Rc};
type Env = crate::env::Env<u32, Value>;
type Joins = BTreeMap<u32, Rc<Join>>;
#[derive(Clone, Debug)]
/// Strict data, lexical closures, and partial applications.
pub enum Value {
    Lit(Lit),
    Con(ConId, Rc<Vec<Value>>, Option<Vec<String>>),
    Closure(Rc<Closure>),
    Pap(Rc<Value>, Vec<Value>),
}

// A constructor list or a chain of captured environments may be arbitrarily deep.
impl Drop for Value {
    fn drop(&mut self) {
        fn children(value: &mut Value, todo: &mut Vec<Value>) {
            match value {
                Value::Con(_, fields, _) => {
                    if !fields.is_empty()
                        && let Ok(values) = Rc::try_unwrap(
                            std::mem::replace(fields, Rc::new(vec![])),
                        )
                    {
                        todo.extend(values);
                    }
                }
                Value::Pap(base, args) => {
                    todo.append(args);
                    if let Some(value) = Rc::get_mut(base) {
                        todo.push(std::mem::replace(
                            value,
                            Value::Lit(Lit::Int(0)),
                        ));
                    }
                }
                Value::Closure(c) => {
                    if let Some(c) = Rc::get_mut(c)
                        && let Some(env) = Rc::get_mut(&mut c.env)
                    {
                        env.drain_unique(todo);
                    }
                }
                _ => {}
            }
        }
        let mut todo = vec![];
        children(self, &mut todo);
        while let Some(mut value) = todo.pop() {
            children(&mut value, &mut todo);
        }
    }
}
#[derive(Clone, Debug)]
/// Recursive groups are shared by index, avoiding reference-count cycles.
pub struct Closure {
    lambda: Lambda,
    env: Rc<Env>,
    recursive: Option<usize>,
}
#[derive(Clone)]
struct Join {
    params: Vec<Var>,
    body: Body,
    env: Env,
    joins: Joins,
}
enum Frame {
    Bind(Var, Body, Env, Joins),
    Apply(Vec<Value>),
}
enum State {
    Code(Body),
    Apply(Value, Vec<Value>),
    Return(Value),
}
#[derive(Clone, Copy, Debug, Default)]
/// Continuation depth counts pending non-tail calls, not recursive tail transfers.
pub struct Stats {
    pub steps: usize,
    pub max_continuations: usize,
}
struct Machine<'a> {
    groups: Vec<Env>,
    program: &'a anf::Program,
    globals: Env,
    env: Env,
    joins: Joins,
    stack: Vec<Frame>,
    fuel: usize,
    stats: Stats,
}
fn key(v: &Var) -> u32 {
    v.name.unique
}
fn closure(
    lambda: Lambda,
    env: Rc<Env>,
    recursive: Option<usize>,
) -> Value {
    Value::Closure(Rc::new(Closure {
        lambda,
        env,
        recursive,
    }))
}
fn function(body: &Body) -> Option<Lambda> {
    if let Anf::Let(v, Rhs::Lambda(l), b) = body.as_ref()
        && matches!(b.as_ref(),Anf::Ret(Atom::Var(w)) if v==w)
    {
        Some(l.clone())
    } else {
        None
    }
}
impl Machine<'_> {
    fn atom(&self, a: &Atom) -> Result<Value, Failure> {
        match a {
            Atom::Lit(l) => Ok(Value::Lit(l.clone())),
            Atom::Var(v) => {
                self.env.get(&key(v)).cloned().ok_or_else(|| {
                    format!("internal error: unbound ANF variable {v}")
                        .into()
                })
            }
            Atom::Global(v) => {
                self.globals.get(&key(v)).cloned().ok_or_else(|| {
                    format!("internal error: uninitialized ANF global {v}")
                        .into()
                })
            }
        }
    }
    fn atoms(&self, args: &[Atom]) -> Result<Vec<Value>, Failure> {
        args.iter().map(|a| self.atom(a)).collect()
    }
    fn rhs(&self, r: &Rhs) -> Result<Value, Failure> {
        match r {
            Rhs::Atom(a) => self.atom(a),
            Rhs::Con(c, args) => Ok(Value::Con(
                c.clone(),
                Rc::new(self.atoms(args)?),
                self.program.metadata.constructors[c.index as usize]
                    .labels
                    .clone(),
            )),
            Rhs::Lambda(l) => {
                Ok(closure(l.clone(), Rc::new(self.env.clone()), None))
            }
            Rhs::Field(a, i) => match &self.atom(a)? {
                Value::Con(_, fields, _) => {
                    fields.get(*i as usize).cloned().ok_or_else(|| {
                        "internal error: invalid field index".into()
                    })
                }
                _ => Err("internal error: field on non-constructor".into()),
            },
            Rhs::Prim(op, args, _) => {
                let p = match op {
                    Op::Runtime(p) => *p,
                    Op::Add => crate::primitives::Prim::IntAdd,
                    Op::Sub => crate::primitives::Prim::IntSub,
                    Op::Mul => crate::primitives::Prim::IntMul,
                    Op::Div => crate::primitives::Prim::IntDiv,
                    Op::Mod => crate::primitives::Prim::IntMod,
                    Op::Eq => crate::primitives::Prim::IntEq,
                    Op::Lt => crate::primitives::Prim::IntLt,
                    Op::Le => crate::primitives::Prim::IntLe,
                    Op::Append => crate::primitives::Prim::StrAppend,
                };
                let values = self.atoms(args)?;
                if p == crate::primitives::Prim::Trace {
                    let Value::Lit(Lit::String(message)) = &values[0]
                    else {
                        return Err(
                            "internal error: trace expects String".into()
                        );
                    };
                    fun_rt::trace(message)
                        .map_err(|e| Failure::Runtime(e.to_string()))?;
                    return Ok(values[1].clone());
                }
                let args = values
                    .iter()
                    .map(to_core)
                    .collect::<Result<Vec<_>, _>>()?;
                crate::primitives::evaluate(
                    p,
                    &args,
                    &self.program.metadata,
                )
                .map(from_core)
            }
            Rhs::Call(..) => unreachable!("calls are machine transitions"),
        }
    }
    fn run(&mut self, mut state: State) -> Result<Value, Failure> {
        loop {
            if self.fuel == 0 {
                return Err("step limit exceeded".into());
            }
            self.fuel -= 1;
            self.stats.steps += 1;
            self.stats.max_continuations =
                self.stats.max_continuations.max(self.stack.len());
            state = match state {
                State::Code(body) => match body.as_ref() {
                    Anf::Ret(a) => State::Return(self.atom(a)?),
                    Anf::TailCall(f, args) => {
                        State::Apply(self.atom(f)?, self.atoms(args)?)
                    }
                    Anf::Let(v, Rhs::Call(f, args), rest) => {
                        let f = self.atom(f)?;
                        let args = self.atoms(args)?;
                        self.stack.push(Frame::Bind(
                            v.clone(),
                            rest.clone(),
                            std::mem::take(&mut self.env),
                            std::mem::take(&mut self.joins),
                        ));
                        State::Apply(f, args)
                    }
                    Anf::Let(v, r, rest) => {
                        let value = self.rhs(r)?;
                        self.env.insert(key(v), value);
                        State::Code(rest.clone())
                    }
                    Anf::LetRec(bindings, rest) => {
                        let index = self.groups.len();
                        let env = Rc::new(self.env.clone());
                        for (v, l) in bindings {
                            self.env.insert(
                                key(v),
                                closure(
                                    l.clone(),
                                    env.clone(),
                                    Some(index),
                                ),
                            );
                        }
                        self.groups.push(self.env.clone());
                        State::Code(rest.clone())
                    }
                    Anf::Join(j, params, body, rest) => {
                        let join = Join {
                            params: params.clone(),
                            body: body.clone(),
                            env: self.env.clone(),
                            joins: self.joins.clone(),
                        };
                        self.joins.insert(key(j), Rc::new(join));
                        State::Code(rest.clone())
                    }
                    Anf::Jump(j, args) => {
                        let values = self.atoms(args)?;
                        let join =
                            self.joins.get(&key(j)).cloned().ok_or(
                                "internal error: unbound ANF join",
                            )?;
                        if values.len() != join.params.len() {
                            return Err(
                                "internal error: ANF jump arity".into()
                            );
                        }
                        self.env = join.env.clone();
                        self.joins = join.joins.clone();
                        self.env.extend(
                            join.params.iter().map(key).zip(values),
                        );
                        State::Code(join.body.clone())
                    }
                    Anf::Case(a, alts) => {
                        let value = self.atom(a)?;
                        let mut chosen = None;
                        for (c, ps, b) in alts {
                            let fields = match (c, &value) {
                                (AltCon::Default, _) => Some(vec![]),
                                (
                                    AltCon::Con(c),
                                    Value::Con(d, fields, _),
                                ) if c == d => Some((**fields).clone()),
                                (AltCon::Lit(a), Value::Lit(b))
                                    if a == b =>
                                {
                                    Some(vec![])
                                }
                                _ => None,
                            };
                            if let Some(fields) = fields {
                                self.env
                                    .extend(ps.iter().map(key).zip(fields));
                                chosen = Some(b.clone());
                                break;
                            }
                        }
                        State::Code(chosen.ok_or(
                            "internal error: ANF case has no alternative",
                        )?)
                    }
                    Anf::Fail(span) => {
                        let site = self
                            .program
                            .metadata
                            .locations
                            .iter()
                            .find(|(s, _)| s == span)
                            .map(|(_, s)| s.as_str())
                            .unwrap_or("<unknown>:1:1");
                        return Err(format!(
                            "pattern match failure at {site}"
                        )
                        .into());
                    }
                },
                State::Apply(f, args) => match &f {
                    Value::Pap(base, earlier) => {
                        let mut earlier = earlier.clone();
                        earlier.extend(args);
                        State::Apply((**base).clone(), earlier)
                    }
                    Value::Closure(c) => {
                        let arity = c.lambda.params.len();
                        if args.len() < arity {
                            State::Return(Value::Pap(
                                Rc::new(Value::Closure(c.clone())),
                                args,
                            ))
                        } else {
                            let mut args = args;
                            let extra = args.split_off(arity);
                            if !extra.is_empty() {
                                self.stack.push(Frame::Apply(extra));
                            }
                            self.env = (*c.env).clone();
                            self.joins.clear();
                            if let Some(index) = c.recursive {
                                self.env = self.groups[index].clone();
                            }
                            self.env.extend(
                                c.lambda.params.iter().map(key).zip(args),
                            );
                            State::Code(c.lambda.body.clone())
                        }
                    }
                    _ => {
                        return Err(
                            "internal error: application of non-function"
                                .into(),
                        );
                    }
                },
                State::Return(value) => match self.stack.pop() {
                    None => return Ok(value),
                    Some(Frame::Apply(args)) => State::Apply(value, args),
                    Some(Frame::Bind(v, b, mut env, joins)) => {
                        env.insert(key(&v), value);
                        self.env = env;
                        self.joins = joins;
                        State::Code(b)
                    }
                },
            };
        }
    }
}
/// Independent control flow, with shared monomorphic primitive definitions.
pub fn run(
    program: &anf::Program,
    fuel: usize,
    io: bool,
) -> Result<(Value, Stats), Failure> {
    let mut m = Machine {
        groups: vec![],
        program,
        globals: Env::new(),
        env: Env::new(),
        joins: Joins::new(),
        stack: vec![],
        fuel,
        stats: Stats::default(),
    };
    for (v, b) in &program.globals {
        if let Some(l) = function(b) {
            m.globals
                .insert(key(v), closure(l, Rc::new(Env::new()), None));
        }
    }
    for (v, b) in &program.globals {
        if function(b).is_none() {
            m.env.clear();
            m.joins.clear();
            let value = m.run(State::Code(b.clone()))?;
            m.globals.insert(key(v), value);
        }
    }
    let main = m
        .globals
        .get(&program.metadata.main.unique())
        .cloned()
        .ok_or("internal error: missing main")?;
    let value = if io {
        let unit = program
            .metadata
            .constructors
            .iter()
            .find(|c| c.id.name == "()")
            .ok_or("internal error: missing unit")?;
        m.run(State::Apply(
            main,
            vec![Value::Con(unit.id.clone(), Rc::new(vec![]), None)],
        ))?
    } else {
        main
    };
    Ok((value, m.stats))
}
fn to_core(v: &Value) -> Result<crate::eval::Value, Failure> {
    Ok(match v {
        Value::Lit(l) => crate::eval::Value::Lit(l.clone()),
        Value::Con(c, fields, labels) => crate::eval::Value::Con(
            c.clone(),
            Rc::new(fields.iter().map(to_core).collect::<Result<_, _>>()?),
            labels.clone(),
        ),
        _ => {
            return Err(
                "internal error: function passed to a data primitive"
                    .into(),
            );
        }
    })
}
fn from_core(v: crate::eval::Value) -> Value {
    match &v {
        crate::eval::Value::Lit(l) => Value::Lit(l.clone()),
        crate::eval::Value::Con(c, fields, labels) => Value::Con(
            c.clone(),
            Rc::new(fields.iter().cloned().map(from_core).collect()),
            labels.clone(),
        ),
        _ => unreachable!("primitive cannot return a closure"),
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
            Self::Closure(_) | Self::Pap(..) => write!(f, "<function>"),
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
