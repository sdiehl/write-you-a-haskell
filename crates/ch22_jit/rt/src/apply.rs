//! Eval/apply: exact calls enter code, short calls allocate PAPs, long calls continue.
use crate::{
    heap::{self, Roots},
    layout::{self, Word},
};
/// The slow entry receives its closure word and exactly its syntactic arity of arguments.
pub type Slow = unsafe extern "C" fn(Word, *const Word) -> Word;
/// Store a code address, syntactic arity, then captured values.
pub fn closure(code: Slow, arity: usize, captures: &[Word]) -> Word {
    assert!(arity > 0);
    let mut fields = vec![code as usize as Word, arity as Word];
    fields.extend_from_slice(captures);
    heap::object(layout::CLOSURE, 0, &fields)
}
/// # Safety
/// Arguments must be valid live words of the callee's source types, rooted by the caller.
/// args must point to n readable words (or may be null when n is zero).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rt_make_pap(
    clo: Word,
    n: usize,
    args: *const Word,
) -> Word {
    // SAFETY: caller supplies a readable argument array and a live function value.
    unsafe {
        let args = arguments(n, args);
        let mut fields = vec![clo, n as Word];
        fields.extend_from_slice(args);
        heap::object(layout::PAP, 0, &fields)
    }
}
unsafe fn arguments<'a>(n: usize, args: *const Word) -> &'a [Word] {
    if n == 0 {
        &[]
    } else {
        // SAFETY: the enclosing ABI contract requires n initialized, aligned words.
        unsafe { std::slice::from_raw_parts(args, n) }
    }
}
/// # Safety
/// clo must be a live CLOSURE and index a capture slot; value must be live and well typed.
/// Patching is confined to unpublished letrec initialization; no concurrent readers exist.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rt_closure_set(
    clo: Word,
    index: usize,
    value: Word,
) {
    // SAFETY: caller guarantees exclusive initialization; checked index excludes code/arity.
    unsafe {
        let h = heap::header(clo);
        assert_eq!(layout::kind(h), layout::CLOSURE);
        let i = index
            .checked_add(layout::CLOSURE_CAPTURES)
            .expect("capture overflow");
        assert!(i < layout::size(h));
        (clo as *mut Word).add(i + 1).write(value);
    }
}
/// # Safety
/// clo is a live closure or PAP; args is n valid, rooted, correctly typed words.
/// Every slow entry must obey the runtime ABI and return a live word of its result type.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rt_apply(
    mut clo: Word,
    n: usize,
    args: *const Word,
) -> Word {
    // SAFETY: all raw accesses follow the caller's typed-object contract and checked headers.
    unsafe {
        let mut roots = Roots::new(arguments(n, args));
        loop {
            let h = heap::header(clo);
            match layout::kind(h) {
                layout::PAP => {
                    let count =
                        heap::field(clo, layout::PAP_COUNT) as usize;
                    assert_eq!(layout::size(h), count + layout::PAP_ARGS);
                    let mut all = (0..count)
                        .map(|i| heap::field(clo, layout::PAP_ARGS + i))
                        .collect::<Vec<_>>();
                    all.extend_from_slice(roots.as_slice());
                    roots = Roots::new(&all);
                    clo = heap::field(clo, layout::PAP_CLOSURE);
                }
                layout::CLOSURE => {
                    let arity =
                        heap::field(clo, layout::CLOSURE_ARITY) as usize;
                    assert!(arity > 0);
                    if roots.as_slice().len() < arity {
                        return rt_make_pap(
                            clo,
                            roots.as_slice().len(),
                            roots.as_slice().as_ptr(),
                        );
                    }
                    let address = heap::field(clo, layout::CLOSURE_CODE);
                    // SAFETY: closure construction stores a Slow address, validated by the ABI contract.
                    let code: Slow = std::mem::transmute::<usize, Slow>(
                        address as usize,
                    );
                    let result = code(clo, roots.as_slice().as_ptr());
                    if roots.as_slice().len() == arity {
                        return result;
                    }
                    roots = Roots::new(&roots.as_slice()[arity..]);
                    clo = result;
                }
                _ => panic!("runtime application of non-function"),
            }
        }
    }
}
macro_rules! apply_n { ($name:ident; $($arg:ident),+) => {
    /// Fixed-size apply entry point.
    /// # Safety
    /// The closure and argument words must satisfy rt_apply's typed, live-object contract.
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn $name(clo: Word, $($arg: Word),+) -> Word {
        let args = [$($arg),+];
        // SAFETY: the local array contains the caller's valid words for the duration of apply.
        unsafe { rt_apply(clo, args.len(), args.as_ptr()) }
    }
}; }
apply_n!(rt_apply1; a);
apply_n!(rt_apply2; a,b);
apply_n!(rt_apply3; a,b,c);
apply_n!(rt_apply4; a,b,c,d);
