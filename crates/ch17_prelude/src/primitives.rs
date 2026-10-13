//! Monomorphic primitive signatures and the reference implementations of pure operations.
use crate::{
    ast::Op,
    core::{self, Lit},
    data::DataEnv,
    eval::Value,
    types::{Scheme, Type},
};

macro_rules! primitives {($($variant:ident => $name:literal),* $(,)?)=>{
    #[derive(Clone,Copy,Debug,PartialEq,Eq)] pub enum Prim {$($variant),*}
    pub const ALL:&[Prim]=&[$(Prim::$variant),*];
    impl Prim {pub fn name(self)->&'static str {match self {$(Self::$variant=>$name),*}}}
};}

primitives! {
    IntAdd=>"prim_intAdd",IntSub=>"prim_intSub",IntMul=>"prim_intMul",IntNeg=>"prim_intNeg",IntQuot=>"prim_intQuot",IntRem=>"prim_intRem",IntDiv=>"prim_intDiv",IntMod=>"prim_intMod",IntEq=>"prim_intEq",IntLt=>"prim_intLt",IntLe=>"prim_intLe",
    DblAdd=>"prim_dblAdd",DblSub=>"prim_dblSub",DblMul=>"prim_dblMul",DblDiv=>"prim_dblDiv",DblNeg=>"prim_dblNeg",DblSqrt=>"prim_dblSqrt",DblExp=>"prim_dblExp",DblLog=>"prim_dblLog",DblSin=>"prim_dblSin",DblCos=>"prim_dblCos",DblPow=>"prim_dblPow",DblEq=>"prim_dblEq",DblLt=>"prim_dblLt",DblLe=>"prim_dblLe",
    IntToDbl=>"prim_intToDbl",DblFloor=>"prim_dblFloor",DblRound=>"prim_dblRound",DblTruncate=>"prim_dblTruncate",ChrOrd=>"prim_chrOrd",ChrChr=>"prim_chrChr",ChrEq=>"prim_chrEq",ChrLt=>"prim_chrLt",
    StrAppend=>"prim_strAppend",StrLength=>"prim_strLength",StrEq=>"prim_strEq",StrCompare=>"prim_strCompare",StrUnpack=>"prim_strUnpack",StrPack=>"prim_strPack",
    ShowInt=>"prim_showInt",ShowDouble=>"prim_showDouble",ShowChar=>"prim_showChar",ShowString=>"prim_showString",ReadInt=>"prim_readInt",Error=>"prim_error",PutStr=>"prim_putStr",GetLine=>"prim_getLine",Exit=>"prim_exit"
}

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
    if matches!(op, Op::Runtime(Error | Exit)) {
        scheme.variables = vec![0];
        scheme.kinds = vec![crate::kinds::Kind::Star];
    }
    scheme
}

fn normalize(n: i64) -> i64 {
    (n << 1) >> 1
}

/// Formatting starts from Rust's shortest round-trip digits, then normalizes notation.
pub fn show_double(n: f64) -> String {
    if n.is_nan() {
        return "NaN".into();
    }
    if n.is_infinite() {
        return if n.is_sign_negative() {
            "-Infinity"
        } else {
            "Infinity"
        }
        .into();
    }
    if n == 0.0 {
        return if n.is_sign_negative() { "-0.0" } else { "0.0" }.into();
    }
    let raw = format!("{:?}", n.abs());
    let (mantissa, exponent) = raw
        .split_once('e')
        .map_or((raw.as_str(), 0), |(a, b)| (a, b.parse::<i32>().unwrap()));
    let point =
        mantissa.find('.').unwrap_or(mantissa.len()) as i32 + exponent;
    let digits = mantissa.replace('.', "");
    let leading = digits.len() - digits.trim_start_matches('0').len();
    let digits = digits.trim_start_matches('0').trim_end_matches('0');
    let point = point - leading as i32;
    let body = if (1e-4..1e16).contains(&n.abs()) {
        if point <= 0 {
            format!("0.{}{digits}", "0".repeat((-point) as usize))
        } else if point as usize >= digits.len() {
            format!(
                "{digits}{}.0",
                "0".repeat(point as usize - digits.len())
            )
        } else {
            format!(
                "{}.{}",
                &digits[..point as usize],
                &digits[point as usize..]
            )
        }
    } else {
        format!(
            "{}.{}e{:+}",
            &digits[..1],
            if digits.len() == 1 { "0" } else { &digits[1..] },
            point - 1
        )
    };
    if n.is_sign_negative() {
        format!("-{body}")
    } else {
        body
    }
}

/// Primitive execution returns the language error text, without a process exit.
pub fn evaluate(
    p: Prim,
    args: &[Value],
    program: &core::Program,
) -> Result<Value, crate::eval::Failure> {
    use Prim::*;
    let int = |i| match args[i] {
        Value::Lit(Lit::Int(n)) => n,
        _ => unreachable!("typed primitive"),
    };
    let dbl = |i| match args[i] {
        Value::Lit(Lit::Double(n)) => f64::from_bits(n),
        _ => unreachable!("typed primitive"),
    };
    let chr = |i| match args[i] {
        Value::Lit(Lit::Char(c)) => c,
        _ => unreachable!("typed primitive"),
    };
    let string = |i| match &args[i] {
        Value::Lit(Lit::String(s)) => s.as_str(),
        _ => unreachable!("typed primitive"),
    };
    let integer = |n| Value::Lit(Lit::Int(normalize(n)));
    let double = |n: f64| Value::Lit(Lit::Double(n.to_bits()));
    let text = |s| Value::Lit(Lit::String(s));
    let con = |name: &str, fields: Vec<Value>| {
        let c = program
            .constructors
            .iter()
            .find(|c| c.id.name == name)
            .expect("primitive constructor");
        Value::Con(c.id.clone(), fields.into(), c.labels.clone())
    };
    let boolean = |b| con(if b { "True" } else { "False" }, vec![]);
    Ok(match p {
        IntAdd => integer(int(0).wrapping_add(int(1))),
        IntSub => integer(int(0).wrapping_sub(int(1))),
        IntMul => integer(int(0).wrapping_mul(int(1))),
        IntNeg => integer(-int(0)),
        IntQuot | IntRem | IntDiv | IntMod => {
            let (a, b) = (int(0), int(1));
            if b == 0 {
                return Err("divide by zero".into());
            }
            let (mut q, mut r) = (a / b, a % b);
            if matches!(p, IntDiv | IntMod) && r != 0 && (r < 0) != (b < 0)
            {
                q -= 1;
                r += b;
            }
            integer(if matches!(p, IntQuot | IntDiv) { q } else { r })
        }
        IntEq => boolean(int(0) == int(1)),
        IntLt => boolean(int(0) < int(1)),
        IntLe => boolean(int(0) <= int(1)),
        DblAdd => double(dbl(0) + dbl(1)),
        DblSub => double(dbl(0) - dbl(1)),
        DblMul => double(dbl(0) * dbl(1)),
        DblDiv => double(dbl(0) / dbl(1)),
        DblPow => double(dbl(0).powf(dbl(1))),
        DblNeg => double(-dbl(0)),
        DblSqrt => double(dbl(0).sqrt()),
        DblExp => double(dbl(0).exp()),
        DblLog => double(dbl(0).ln()),
        DblSin => double(dbl(0).sin()),
        DblCos => double(dbl(0).cos()),
        DblEq => boolean(dbl(0) == dbl(1)),
        DblLt => boolean(dbl(0) < dbl(1)),
        DblLe => boolean(dbl(0) <= dbl(1)),
        IntToDbl => double(int(0) as f64),
        DblFloor | DblRound | DblTruncate => {
            let n = match p {
                DblFloor => dbl(0).floor(),
                DblRound => dbl(0).round_ties_even(),
                _ => dbl(0).trunc(),
            };
            integer(if n.is_nan() {
                0
            } else {
                (n as i64).clamp(-(1_i64 << 62), (1_i64 << 62) - 1)
            })
        }
        ChrOrd => integer(chr(0) as i64),
        ChrChr => {
            let c = u32::try_from(int(0))
                .ok()
                .and_then(char::from_u32)
                .ok_or("invalid character code point")?;
            Value::Lit(Lit::Char(c))
        }
        ChrEq => boolean(chr(0) == chr(1)),
        ChrLt => boolean(chr(0) < chr(1)),
        StrAppend => text(format!("{}{}", string(0), string(1))),
        StrLength => integer(string(0).chars().count() as i64),
        StrEq => boolean(string(0) == string(1)),
        StrCompare => integer(match string(0).cmp(string(1)) {
            std::cmp::Ordering::Less => -1,
            std::cmp::Ordering::Equal => 0,
            std::cmp::Ordering::Greater => 1,
        }),
        StrUnpack => {
            string(0).chars().rev().fold(con("[]", vec![]), |xs, c| {
                con(":", vec![Value::Lit(Lit::Char(c)), xs])
            })
        }
        StrPack => {
            let mut s = String::new();
            let mut xs = &args[0];
            while let Value::Con(id, fields, _) = xs {
                if id.name == "[]" {
                    break;
                }
                let Value::Lit(Lit::Char(c)) = fields[0] else {
                    unreachable!()
                };
                s.push(c);
                xs = &fields[1];
            }
            text(s)
        }
        ShowInt => text(int(0).to_string()),
        ShowDouble => text(show_double(dbl(0))),
        ShowChar => text(crate::ast::quote(&chr(0).to_string(), '\'')),
        ShowString => text(crate::ast::quote(string(0), '"')),
        ReadInt => {
            let s = string(0).trim();
            let digits = s.strip_prefix(['+', '-']).unwrap_or(s);
            let parsed = if !digits.is_empty()
                && digits.bytes().all(|b| b.is_ascii_digit())
            {
                s.parse::<i64>()
                    .ok()
                    .filter(|n| (-(1_i64 << 62)..(1_i64 << 62)).contains(n))
            } else {
                None
            };
            match parsed {
                Some(n) => con("Just", vec![integer(n)]),
                None => con("Nothing", vec![]),
            }
        }
        Error => return Err(string(0).into()),
        PutStr => {
            use std::io::Write;
            std::io::stdout()
                .write_all(string(0).as_bytes())
                .map_err(|e| e.to_string())?;
            con("()", vec![])
        }
        GetLine => {
            use std::io::Write;
            std::io::stdout().flush().map_err(|e| e.to_string())?;
            let mut line = String::new();
            if std::io::stdin()
                .read_line(&mut line)
                .map_err(|e| e.to_string())?
                == 0
            {
                return Err("end of input".into());
            }
            if line.ends_with('\n') {
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
            }
            text(line)
        }
        Exit => return Err(crate::eval::Failure::Exit(int(0) as i32)),
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
