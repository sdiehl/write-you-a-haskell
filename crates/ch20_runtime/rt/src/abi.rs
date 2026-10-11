//! C exports decode tagged words, call the shared semantics, and encode the result.
use crate::{
    Failure, Prim, Value, heap,
    layout::{self, Word},
};
use std::{
    ffi::{CStr, c_char},
    io::Write,
};
#[derive(Clone, Copy)]
enum Input {
    Int,
    Double,
    Char,
    String,
    Chars,
    Unit,
}
fn inputs(p: Prim) -> &'static [Input] {
    use Input::*;
    match p {
        Prim::IntAdd => &[Int, Int],
        Prim::IntSub => &[Int, Int],
        Prim::IntMul => &[Int, Int],
        Prim::IntNeg => &[Int],
        Prim::IntQuot => &[Int, Int],
        Prim::IntRem => &[Int, Int],
        Prim::IntDiv => &[Int, Int],
        Prim::IntMod => &[Int, Int],
        Prim::IntEq => &[Int, Int],
        Prim::IntLt => &[Int, Int],
        Prim::IntLe => &[Int, Int],
        Prim::DblAdd => &[Double, Double],
        Prim::DblSub => &[Double, Double],
        Prim::DblMul => &[Double, Double],
        Prim::DblDiv => &[Double, Double],
        Prim::DblNeg => &[Double],
        Prim::DblSqrt => &[Double],
        Prim::DblExp => &[Double],
        Prim::DblLog => &[Double],
        Prim::DblSin => &[Double],
        Prim::DblCos => &[Double],
        Prim::DblPow => &[Double, Double],
        Prim::DblEq => &[Double, Double],
        Prim::DblLt => &[Double, Double],
        Prim::DblLe => &[Double, Double],
        Prim::IntToDbl => &[Int],
        Prim::DblFloor => &[Double],
        Prim::DblRound => &[Double],
        Prim::DblTruncate => &[Double],
        Prim::ChrOrd => &[Char],
        Prim::ChrChr => &[Int],
        Prim::ChrEq => &[Char, Char],
        Prim::ChrLt => &[Char, Char],
        Prim::StrAppend => &[String, String],
        Prim::StrLength => &[String],
        Prim::StrEq => &[String, String],
        Prim::StrCompare => &[String, String],
        Prim::StrUnpack => &[String],
        Prim::StrPack => &[Chars],
        Prim::ShowInt => &[Int],
        Prim::ShowDouble => &[Double],
        Prim::ShowChar => &[Char],
        Prim::ShowString => &[String],
        Prim::ReadInt => &[String],
        Prim::Error => &[String],
        Prim::PutStr => &[String],
        Prim::GetLine => &[Unit],
        Prim::Exit => &[Int],
    }
}
unsafe fn decode(kind: Input, w: Word) -> Value {
    // SAFETY: ABI arguments have the specified source types; heap accessors check object kinds.
    unsafe {
        match kind {
            Input::Int => Value::Int(layout::signed(w)),
            Input::Double => Value::Double(heap::unbox_double(w)),
            Input::Char => Value::Char(
                char::from_u32(layout::signed(w) as u32)
                    .expect("runtime scalar"),
            ),
            Input::String => Value::String(heap::text(w).to_owned()),
            Input::Unit => {
                assert_eq!(w, layout::UNIT);
                Value::Unit
            }
            Input::Chars => {
                let mut chars = vec![];
                let mut xs = w;
                while xs != layout::NIL {
                    let h = heap::header(xs);
                    assert_eq!(layout::kind(h), layout::CON);
                    assert_eq!(layout::tag(h), layout::CONS_TAG);
                    assert_eq!(layout::size(h), 2);
                    chars.push(
                        char::from_u32(
                            layout::signed(heap::field(xs, 0)) as u32
                        )
                        .expect("runtime scalar"),
                    );
                    xs = heap::field(xs, 1);
                }
                Value::Chars(chars)
            }
        }
    }
}
fn encode(v: Value) -> Word {
    match v {
        Value::Int(n) => layout::integer(n),
        Value::Double(n) => heap::double(n),
        Value::Char(c) => layout::integer(c as i64),
        Value::String(s) => heap::string(&s),
        Value::Bool(b) => {
            if b {
                layout::TRUE
            } else {
                layout::FALSE
            }
        }
        Value::Unit => layout::UNIT,
        Value::MaybeInt(n) => match n {
            None => layout::NOTHING,
            Some(n) => {
                heap::constructor(layout::JUST_TAG, &[layout::integer(n)])
            }
        },
        Value::Chars(cs) => {
            cs.into_iter().rev().fold(layout::NIL, |xs, c| {
                heap::constructor(
                    layout::CONS_TAG,
                    &[layout::integer(c as i64), xs],
                )
            })
        }
    }
}
unsafe fn invoke(p: Prim, args: &[Word]) -> Word {
    assert_eq!(inputs(p).len(), args.len());
    // SAFETY: each export promises correctly typed live words for its primitive.
    let values = inputs(p)
        .iter()
        .zip(args)
        .map(|(k, w)| unsafe { decode(*k, *w) })
        .collect::<Vec<_>>();
    match crate::evaluate(p, &values) {
        Ok(v) => encode(v),
        Err(Failure::Runtime(s)) => die(&s),
        Err(Failure::Exit(n)) => rt_exit(n),
    }
}
macro_rules! export { ($name:ident, $op:ident; $($arg:ident),+) => {
    /// Typed primitive entry, using the shared interpreter semantics.
    /// # Safety
    /// Every word must be live and have the primitive's declared source type.
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn $name($($arg: Word),+) -> Word {
        // SAFETY: the exported primitive's contract supplies the exact typed arguments.
        unsafe { invoke(Prim::$op,&[$($arg),+]) }
    }
}; }
export!(rt_prim_intAdd, IntAdd; a,b);
export!(rt_prim_intSub, IntSub; a,b);
export!(rt_prim_intMul, IntMul; a,b);
export!(rt_prim_intNeg, IntNeg; a);
export!(rt_prim_intQuot, IntQuot; a,b);
export!(rt_prim_intRem, IntRem; a,b);
export!(rt_prim_intDiv, IntDiv; a,b);
export!(rt_prim_intMod, IntMod; a,b);
export!(rt_prim_intEq, IntEq; a,b);
export!(rt_prim_intLt, IntLt; a,b);
export!(rt_prim_intLe, IntLe; a,b);
export!(rt_prim_dblAdd, DblAdd; a,b);
export!(rt_prim_dblSub, DblSub; a,b);
export!(rt_prim_dblMul, DblMul; a,b);
export!(rt_prim_dblDiv, DblDiv; a,b);
export!(rt_prim_dblNeg, DblNeg; a);
export!(rt_prim_dblSqrt, DblSqrt; a);
export!(rt_prim_dblExp, DblExp; a);
export!(rt_prim_dblLog, DblLog; a);
export!(rt_prim_dblSin, DblSin; a);
export!(rt_prim_dblCos, DblCos; a);
export!(rt_prim_dblPow, DblPow; a,b);
export!(rt_prim_dblEq, DblEq; a,b);
export!(rt_prim_dblLt, DblLt; a,b);
export!(rt_prim_dblLe, DblLe; a,b);
export!(rt_prim_intToDbl, IntToDbl; a);
export!(rt_prim_dblFloor, DblFloor; a);
export!(rt_prim_dblRound, DblRound; a);
export!(rt_prim_dblTruncate, DblTruncate; a);
export!(rt_prim_chrOrd, ChrOrd; a);
export!(rt_prim_chrChr, ChrChr; a);
export!(rt_prim_chrEq, ChrEq; a,b);
export!(rt_prim_chrLt, ChrLt; a,b);
export!(rt_prim_strAppend, StrAppend; a,b);
export!(rt_prim_strLength, StrLength; a);
export!(rt_prim_strEq, StrEq; a,b);
export!(rt_prim_strCompare, StrCompare; a,b);
export!(rt_prim_strUnpack, StrUnpack; a);
export!(rt_prim_strPack, StrPack; a);
export!(rt_prim_showInt, ShowInt; a);
export!(rt_prim_showDouble, ShowDouble; a);
export!(rt_prim_showChar, ShowChar; a);
export!(rt_prim_showString, ShowString; a);
export!(rt_prim_readInt, ReadInt; a);
export!(rt_prim_error, Error; a);
export!(rt_prim_putStr, PutStr; a);
export!(rt_prim_getLine, GetLine; a);
export!(rt_prim_exit, Exit; a);

fn die(message: &str) -> ! {
    crate::primitives::init_io();
    let _ = std::io::stdout().flush();
    let _ = writeln!(std::io::stderr().lock(), "fun: {message}");
    std::process::exit(1)
}
/// Flush buffered output and terminate with the language's exit status.
#[unsafe(no_mangle)]
pub extern "C" fn rt_exit(code: i32) -> ! {
    if let Err(e) = std::io::stdout().flush() {
        die(&e.to_string());
    }
    std::process::exit(code)
}
/// # Safety
/// message must be a live STRING object.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rt_error(message: Word) -> ! {
    // SAFETY: caller supplies a rooted immutable STRING for this non-returning call.
    unsafe { die(heap::text(message)) }
}
/// # Safety
/// file must address a live NUL-terminated UTF-8 file name.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rt_pattern_fail(
    file: *const c_char,
    line: u32,
    col: u32,
) -> ! {
    // SAFETY: caller supplies a NUL-terminated file-name constant.
    let file = unsafe { CStr::from_ptr(file) }.to_string_lossy();
    die(&format!("pattern match failure at {file}:{line}:{col}"))
}
#[unsafe(no_mangle)]
pub extern "C" fn rt_div_zero() -> ! {
    die("divide by zero")
}
