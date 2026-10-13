//! One implementation of observable primitive behavior for every execution engine.
use crate::layout::normalize;

#[derive(Clone, Debug, PartialEq)]
/// Only primitive input/output types: arbitrary algebraic values stay in each engine.
pub enum Value {
    Int(i64),
    Double(f64),
    Char(char),
    String(String),
    Bool(bool),
    Chars(Vec<char>),
    MaybeInt(Option<i64>),
    Unit,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
            Self::Runtime(s) => f.write_str(s),
            Self::Exit(n) => write!(f, "exit {n}"),
        }
    }
}

/// Optional surrounding whitespace and sign; decimal digits only, within signed 63 bits.
pub fn read_int(source: &str) -> Option<i64> {
    let s = source.trim();
    let digits = s.strip_prefix(['+', '-']).unwrap_or(s);
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    s.parse().ok().filter(|n| {
        (crate::layout::INT_MIN..=crate::layout::INT_MAX).contains(n)
    })
}

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

/// Quote Unicode text using Fun escapes.
pub fn quote(text: &str, delimiter: char) -> String {
    let mut result = String::from(delimiter);
    for c in text.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\t' => result.push_str("\\t"),
            '\0' => result.push_str("\\0"),
            '\\' => result.push_str("\\\\"),
            c if c == delimiter => {
                result.push('\\');
                result.push(c);
            }
            c if c.is_control() => {
                result.push_str(&format!("\\u{{{:x}}}", c as u32))
            }
            c => result.push(c),
        }
    }
    result.push(delimiter);
    result
}

/// Execute a checked primitive, returning errors without terminating the host.
pub fn evaluate(p: Prim, args: &[Value]) -> Result<Value, Failure> {
    use Prim::*;
    let int = |i| match args[i] {
        Value::Int(n) => n,
        _ => unreachable!("typed primitive"),
    };
    let dbl = |i| match args[i] {
        Value::Double(n) => n,
        _ => unreachable!("typed primitive"),
    };
    let chr = |i| match args[i] {
        Value::Char(c) => c,
        _ => unreachable!("typed primitive"),
    };
    let string = |i| match &args[i] {
        Value::String(s) => s.as_str(),
        _ => unreachable!("typed primitive"),
    };
    let integer = |n| Value::Int(normalize(n));
    let double = |n: f64| Value::Double(n);
    let text = |s| Value::String(s);
    let boolean = Value::Bool;
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
            Value::Char(c)
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
        StrUnpack => Value::Chars(string(0).chars().collect()),
        StrPack => match &args[0] {
            Value::Chars(cs) => text(cs.iter().collect()),
            _ => unreachable!("typed character list"),
        },
        ShowInt => text(int(0).to_string()),
        ShowDouble => text(show_double(dbl(0))),
        ShowChar => text(quote(&chr(0).to_string(), '\'')),
        ShowString => text(quote(string(0), '"')),
        ReadInt => Value::MaybeInt(read_int(string(0))),
        Error => return Err(string(0).into()),
        PutStr => {
            use std::io::Write;
            std::io::stdout()
                .write_all(string(0).as_bytes())
                .map_err(|e| e.to_string())?;
            Value::Unit
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
        Exit => return Err(Failure::Exit(int(0) as i32)),
    })
}

/// Match Rust main's broken-pipe policy when entered through a generated C main.
pub(crate) fn init_io() {
    static INIT: std::sync::Once = std::sync::Once::new();
    INIT.call_once(|| {
        // SAFETY: on the supported Unix targets, SIGPIPE is 13 and SIG_IGN is
        // the distinguished handler value 1. This installs no Rust callback.
        unsafe {
            unsafe extern "C" {
                fn signal(number: i32, handler: usize) -> usize;
            }
            signal(13, 1);
        }
    });
}
