//! Boehm allocations and roots for temporary Rust-owned argument buffers.
//! Thread registration follows bdwgc's public gc.h (GC_allow_register_threads).
use crate::layout::{self, Word};
use std::{ffi::c_void, ptr, sync::Once};

#[repr(C)]
struct StackBase {
    mem_base: *mut c_void,
}

// SAFETY: these declarations match bdwgc 8.x gc.h on the supported 64-bit targets.
unsafe extern "C" {
    fn GC_init();
    fn GC_allow_register_threads();
    fn GC_thread_is_registered() -> i32;
    fn GC_get_stack_base(base: *mut StackBase) -> i32;
    fn GC_register_my_thread(base: *const StackBase) -> i32;
    fn GC_unregister_my_thread() -> i32;
    fn GC_malloc(n: usize) -> *mut c_void;
    fn GC_malloc_atomic(n: usize) -> *mut c_void;
    fn GC_add_roots(start: *mut c_void, end: *mut c_void);
    fn GC_remove_roots(start: *mut c_void, end: *mut c_void);
    fn GC_gcollect();
}

static INIT: Once = Once::new();

struct Registration(bool);

impl Drop for Registration {
    fn drop(&mut self) {
        if self.0 {
            // SAFETY: only this thread's successful explicit registration is removed.
            unsafe {
                GC_unregister_my_thread();
            }
        }
    }
}

thread_local! {
    static THREAD: Registration = {
        // SAFETY: INIT completed first; StackBase matches gc.h on our supported targets.
        unsafe {
            if GC_thread_is_registered() != 0 { Registration(false) }
            else {
                let mut base = StackBase { mem_base: ptr::null_mut() };
                assert_eq!(GC_get_stack_base(&mut base), 0, "cannot find GC stack base");
                assert_eq!(GC_register_my_thread(&base), 0, "cannot register GC thread");
                Registration(true)
            }
        }
    };
}

/// Call on the main thread before starting runtime worker threads.
#[unsafe(no_mangle)]
pub extern "C" fn rt_init() {
    crate::primitives::init_io();
    INIT.call_once(|| {
        // SAFETY: initialization is serialized and precedes every heap operation.
        unsafe {
            GC_init();
            GC_allow_register_threads();
        }
    });
    THREAD.with(|_| {});
}

fn checked(p: *mut c_void) -> *mut c_void {
    if p.is_null() {
        eprintln!("fun: out of memory");
        std::process::abort();
    }
    assert_eq!(p as usize % layout::WORD_BYTES, 0);
    p
}

/// Allocate total words, including the header, with pointer scanning enabled.
#[unsafe(no_mangle)]
pub extern "C" fn rt_alloc(words: usize) -> *mut Word {
    rt_init();
    let bytes = words
        .checked_mul(layout::WORD_BYTES)
        .expect("allocation size overflow");
    // SAFETY: the initialized collector returns suitably aligned storage of this size.
    checked(unsafe { GC_malloc(bytes.max(8)) }).cast()
}

/// Allocate bytes for a pointer-free object; callers initialize its header and payload.
#[unsafe(no_mangle)]
pub extern "C" fn rt_alloc_atomic(bytes: usize) -> *mut u8 {
    rt_init();
    // SAFETY: the initialized collector returns aligned storage, never scanned as pointers.
    checked(unsafe { GC_malloc_atomic(bytes.max(8)) }).cast()
}

/// A registered, independently owned range. Rust's ordinary heap is not scanned by Boehm.
pub struct Roots {
    words: Box<[Word]>,
}

impl Roots {
    pub fn new(words: &[Word]) -> Self {
        // Unregistered guard words prevent Boehm merging adjacent root ranges.
        let mut storage = vec![0; words.len() + 2];
        storage[1..words.len() + 1].copy_from_slice(words);
        let words = storage.into_boxed_slice();
        rt_init();
        if words.len() > 2 {
            // SAFETY: this stable, exclusively owned allocation lasts until Drop removes it.
            unsafe {
                GC_add_roots(
                    words.as_ptr().add(1) as *mut c_void,
                    words.as_ptr().add(words.len() - 1) as *mut c_void,
                );
            }
        }
        Self { words }
    }
    pub fn as_slice(&self) -> &[Word] {
        &self.words[1..self.words.len() - 1]
    }
}

impl Drop for Roots {
    fn drop(&mut self) {
        if self.words.len() > 2 {
            // SAFETY: removes exactly the registered range, before its allocation is freed.
            unsafe {
                GC_remove_roots(
                    self.words.as_ptr().add(1) as *mut c_void,
                    self.words.as_ptr().add(self.words.len() - 1)
                        as *mut c_void,
                );
            }
        }
    }
}

/// Force a collection for ABI validation. Live words must be on registered stacks or roots.
pub fn collect() {
    rt_init();
    // SAFETY: the calling thread is registered and the collector owns its heap.
    unsafe {
        GC_gcollect();
    }
}

/// Allocate a scanned object, protecting fields even when supplied in a Rust Vec.
pub fn object(kind: u8, tag: u32, fields: &[Word]) -> Word {
    let roots = Roots::new(fields);
    let size = u32::try_from(fields.len()).expect("object too large");
    let p = rt_alloc(fields.len() + 1);
    // SAFETY: p has header + fields words; protected input and destination do not overlap.
    unsafe {
        p.write(layout::header(kind, tag, size));
        ptr::copy_nonoverlapping(
            roots.as_slice().as_ptr(),
            p.add(1),
            fields.len(),
        );
    }
    p as Word
}

pub fn constructor(tag: u32, fields: &[Word]) -> Word {
    if fields.is_empty() {
        layout::immediate(tag)
    } else {
        object(layout::CON, tag, fields)
    }
}

pub fn double(n: f64) -> Word {
    let p = rt_alloc_atomic(16).cast::<Word>();
    // SAFETY: a DOUBLE consists of one header and one unscanned f64 word.
    unsafe {
        p.write(layout::header(layout::DOUBLE, 0, 1));
        p.add(1).write(n.to_bits());
    }
    p as Word
}

pub fn string(s: &str) -> Word {
    let payload = 1 + s.len().div_ceil(8);
    let size = u32::try_from(payload).expect("string too large");
    let bytes = (payload + 1).checked_mul(8).expect("string size overflow");
    let p = rt_alloc_atomic(bytes);
    // SAFETY: p contains header, byte length, then padded UTF-8 bytes; no pointers are stored.
    unsafe {
        p.write_bytes(0, bytes);
        p.cast::<Word>()
            .write(layout::header(layout::STRING, 0, size));
        p.cast::<Word>().add(1).write(s.len() as Word);
        ptr::copy_nonoverlapping(s.as_ptr(), p.add(16), s.len());
    }
    p as Word
}

/// # Safety
/// w must point to a live, aligned heap object with a valid header.
pub unsafe fn header(w: Word) -> Word {
    assert!(!layout::is_immediate(w) && w != 0);
    // SAFETY: required live object header is the first word.
    unsafe { (w as *const Word).read() }
}

/// # Safety
/// w must be a live object and index must name a payload word.
pub unsafe fn field(w: Word, index: usize) -> Word {
    // SAFETY: the caller guarantees a live object; bounds are checked before reading.
    unsafe {
        assert!(index < layout::size(header(w)));
        (w as *const Word).add(index + 1).read()
    }
}

/// # Safety
/// w must be a live DOUBLE object.
pub unsafe fn unbox_double(w: Word) -> f64 {
    // SAFETY: the object kind and its fixed-size payload are checked.
    unsafe {
        assert_eq!(layout::kind(header(w)), layout::DOUBLE);
        f64::from_bits(field(w, 0))
    }
}

/// # Safety
/// w must be a live STRING, kept rooted for the returned borrow's entire lifetime.
pub unsafe fn text<'a>(w: Word) -> &'a str {
    // SAFETY: caller maintains the object; header/length/UTF-8 are checked before borrowing.
    unsafe {
        let h = header(w);
        assert_eq!(layout::kind(h), layout::STRING);
        let len = field(w, 0) as usize;
        assert!(
            len <= layout::size(h)
                .checked_sub(1)
                .expect("string length word")
                * 8
        );
        std::str::from_utf8(std::slice::from_raw_parts(
            (w as *const u8).add(16),
            len,
        ))
        .expect("runtime UTF-8")
    }
}
