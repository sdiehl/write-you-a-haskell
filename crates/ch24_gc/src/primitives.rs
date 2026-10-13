//! Monomorphic primitive signatures and the reference implementations of pure operations.
use crate::{
    ast::Op,
    core::{self, Lit},
    data::DataEnv,
    eval::Value,
    types::{Scheme, Type},
};
pub use fun_rt::{ALL, Prim, show_double};

/// Every literal and primitive has a fixed type; only error quantifies a result.
pub fn signature(op: Op, data: &DataEnv) -> Scheme {
    use Prim::*;
    let con = |name: &str, t: Type| {
        Type::Con(
            data.tycon(name).expect("Prelude primitive type").id.clone(),
            vec![t],
        )
    };
    let (args, result) = match op {
        Op::Runtime(p) => match p {
            IntAdd | IntSub | IntMul | IntQuot | IntRem | IntDiv
            | IntMod => (vec![Type::Int, Type::Int], Type::Int),
            IntNeg => (vec![Type::Int], Type::Int),
            IntEq | IntLt | IntLe => {
                (vec![Type::Int, Type::Int], Type::Bool)
            }
            DblAdd | DblSub | DblMul | DblDiv | DblPow => {
                (vec![Type::Double, Type::Double], Type::Double)
            }
            DblNeg | DblSqrt | DblExp | DblLog | DblSin | DblCos => {
                (vec![Type::Double], Type::Double)
            }
            DblEq | DblLt | DblLe => {
                (vec![Type::Double, Type::Double], Type::Bool)
            }
            IntToDbl => (vec![Type::Int], Type::Double),
            DblFloor | DblRound | DblTruncate => {
                (vec![Type::Double], Type::Int)
            }
            ChrOrd => (vec![Type::Char], Type::Int),
            ChrChr => (vec![Type::Int], Type::Char),
            ChrEq | ChrLt => (vec![Type::Char, Type::Char], Type::Bool),
            StrAppend => (vec![Type::String, Type::String], Type::String),
            StrLength => (vec![Type::String], Type::Int),
            StrEq => (vec![Type::String, Type::String], Type::Bool),
            StrCompare => (vec![Type::String, Type::String], Type::Int),
            StrUnpack => (vec![Type::String], con("[]", Type::Char)),
            StrPack => (vec![con("[]", Type::Char)], Type::String),
            ShowInt => (vec![Type::Int], Type::String),
            ShowDouble => (vec![Type::Double], Type::String),
            ShowChar => (vec![Type::Char], Type::String),
            ShowString => (vec![Type::String], Type::String),
            ReadInt => (vec![Type::String], con("Maybe", Type::Int)),
            Trace => (vec![Type::String, Type::Var(0)], Type::Var(0)),
            Error => (vec![Type::String], Type::Var(0)),
            Exit => (vec![Type::Int], Type::Var(0)),
            PutStr => (
                vec![Type::String],
                Type::Con(data.tycon("()").unwrap().id.clone(), vec![]),
            ),
            GetLine => (
                vec![Type::Con(
                    data.tycon("()").unwrap().id.clone(),
                    vec![],
                )],
                Type::String,
            ),
        },
        Op::Append => (vec![Type::String, Type::String], Type::String),
        Op::Eq | Op::Lt | Op::Le => {
            (vec![Type::Int, Type::Int], Type::Bool)
        }
        _ => (vec![Type::Int, Type::Int], Type::Int),
    };
    let mut scheme = Scheme::mono(
        args.into_iter()
            .rev()
            .fold(result, |b, a| Type::arrow(a, b)),
    );
    if matches!(op, Op::Runtime(Error | Exit | Trace)) {
        scheme.variables = vec![0];
        scheme.kinds = vec![crate::kinds::Kind::Star];
    }
    scheme
}

/// All interpreters use the same runtime functions; only algebraic values are adapted.
pub fn evaluate(
    p: Prim,
    args: &[Value],
    program: &core::Program,
) -> Result<Value, crate::eval::Failure> {
    if p == Prim::Trace {
        let Value::Lit(Lit::String(message)) = &args[0] else {
            unreachable!("typed trace")
        };
        fun_rt::trace(message)
            .map_err(|e| crate::eval::Failure::Runtime(e.to_string()))?;
        return Ok(args[1].clone());
    }
    use fun_rt::Value as R;
    let args = args
        .iter()
        .map(|v| match v {
            Value::Lit(Lit::Int(n)) => R::Int(*n),
            Value::Lit(Lit::Double(n)) => R::Double(f64::from_bits(*n)),
            Value::Lit(Lit::Char(c)) => R::Char(*c),
            Value::Lit(Lit::String(s)) => R::String(s.clone()),
            Value::Con(id, _, _) if id.name == "()" => R::Unit,
            Value::Con(..) => {
                let mut chars = vec![];
                let mut xs = v;
                while let Value::Con(id, fields, _) = xs {
                    if id.name == "[]" {
                        break;
                    }
                    let Value::Lit(Lit::Char(c)) = fields[0] else {
                        unreachable!("typed character list")
                    };
                    chars.push(c);
                    xs = &fields[1];
                }
                R::Chars(chars)
            }
            _ => unreachable!("data primitive"),
        })
        .collect::<Vec<_>>();
    let result = fun_rt::evaluate(p, &args).map_err(|e| match e {
        fun_rt::Failure::Runtime(s) => crate::eval::Failure::Runtime(s),
        fun_rt::Failure::Exit(n) => crate::eval::Failure::Exit(n),
    })?;
    let con = |name: &str, fields: Vec<Value>| {
        let c = program
            .constructors
            .iter()
            .find(|c| c.id.name == name)
            .expect("primitive constructor");
        Value::Con(c.id.clone(), fields.into(), c.labels.clone())
    };
    Ok(match result {
        R::Int(n) => Value::Lit(Lit::Int(n)),
        R::Double(n) => Value::Lit(Lit::Double(n.to_bits())),
        R::Char(c) => Value::Lit(Lit::Char(c)),
        R::String(s) => Value::Lit(Lit::String(s)),
        R::Bool(b) => con(if b { "True" } else { "False" }, vec![]),
        R::Unit => con("()", vec![]),
        R::Chars(cs) => {
            cs.into_iter().rev().fold(con("[]", vec![]), |xs, c| {
                con(":", vec![Value::Lit(Lit::Char(c)), xs])
            })
        }
        R::MaybeInt(n) => match n {
            Some(n) => con("Just", vec![Value::Lit(Lit::Int(n))]),
            None => con("Nothing", vec![]),
        },
    })
}

/// Fixed primitive types are shared metadata; lint still checks every argument itself.
pub fn core_signature(
    op: Op,
    arguments: &[core::Type],
    heads: &[(crate::data::TyConId, crate::kinds::Kind)],
) -> Result<(core::Type, usize), String> {
    let data = DataEnv {
        types: heads
            .iter()
            .map(|(id, k)| crate::data::TyConInfo {
                id: id.clone(),
                arity: k.parameters().len(),
                kind: k.clone(),
                constructors: vec![],
            })
            .collect(),
        ..DataEnv::default()
    };
    let scheme = signature(op, &data);
    if scheme.variables.len() != arguments.len() {
        return Err("unsaturated Core primitive type arguments".into());
    }
    fn convert(t: &Type, args: &[core::Type]) -> core::Type {
        match t {
            Type::Int => core::Type::Int,
            Type::Double => core::Type::Double,
            Type::Bool => core::Type::Bool,
            Type::Char => core::Type::Char,
            Type::String => core::Type::String,
            Type::Var(v) => args[*v as usize].clone(),
            Type::Con(id, ts) => core::Type::Con(
                id.clone(),
                ts.iter().map(|t| convert(t, args)).collect(),
            ),
            Type::Arrow(a, b) => {
                core::Type::arrow(convert(a, args), convert(b, args))
            }
            _ => unreachable!("primitive signature"),
        }
    }
    let mut arity = 0;
    let mut head = &scheme.ty;
    while let Type::Arrow(_, b) = head {
        arity += 1;
        head = b;
    }
    Ok((convert(&scheme.ty, arguments), arity))
}
