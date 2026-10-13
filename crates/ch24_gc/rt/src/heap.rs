//! Boehm allocations and roots for temporary Rust-owned argument buffers.
//! Thread registration follows bdwgc's public gc.h (GC_allow_register_threads).
use crate::{
    copying,
    layout::{self, Word},
};
use std::ptr;
#[cfg(not(feature = "copying-only"))]
use std::{ffi::c_void, sync::Once};

#[cfg(not(feature = "copying-only"))]
#[repr(C)]
struct StackBase {
    mem_base: *mut c_void,
}

// SAFETY: these declarations match bdwgc 8.x gc.h on the supported 64-bit targets.
#[cfg(not(feature = "copying-only"))]
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

#[cfg(not(feature = "copying-only"))]
static INIT: Once = Once::new();

#[cfg(not(feature = "copying-only"))]
struct Registration(bool);

#[cfg(not(feature = "copying-only"))]
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

#[cfg(not(feature = "copying-only"))]
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
    #[cfg(feature = "copying-only")]
    if !copying::enabled() {
        copying::rt_use_copying();
    }
    #[cfg(not(feature = "copying-only"))]
    {
        INIT.call_once(|| {
            // SAFETY: initialization is serialized and precedes every heap operation.
            unsafe {
                GC_init();
                GC_allow_register_threads();
            }
        });
        THREAD.with(|_| {});
    }
}

#[cfg(not(feature = "copying-only"))]
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
    if copying::enabled() {
        assert!(words >= 2, "heap objects need a forwarding slot");
        return copying::allocate(words);
    }
    #[cfg(feature = "copying-only")]
    panic!("copying collector must be initialized before allocation");
    #[cfg(not(feature = "copying-only"))]
    {
        rt_init();
        let bytes = words
            .checked_mul(layout::WORD_BYTES)
            .expect("allocation size overflow");
        // SAFETY: the initialized collector returns suitably aligned storage of this size.
        checked(unsafe { GC_malloc(bytes.max(8)) }).cast()
    }
}

/// Allocate bytes for a pointer-free object; callers initialize its header and payload.
#[unsafe(no_mangle)]
pub extern "C" fn rt_alloc_atomic(bytes: usize) -> *mut u8 {
    if copying::enabled() {
        assert!(bytes > 8, "heap objects need a forwarding slot");
        return copying::allocate(bytes.div_ceil(8)).cast();
    }
    #[cfg(feature = "copying-only")]
    panic!("copying collector must be initialized before allocation");
    #[cfg(not(feature = "copying-only"))]
    {
        rt_init();
        // SAFETY: the initialized collector returns aligned storage, never scanned as pointers.
        checked(unsafe { GC_malloc_atomic(bytes.max(8)) }).cast()
    }
}

/// A registered, independently owned range. Rust's ordinary heap is not scanned by Boehm.
pub struct Roots {
    words: Box<[std::cell::UnsafeCell<Word>]>,
    thread: std::marker::PhantomData<std::rc::Rc<()>>,
}

impl Roots {
    pub fn new(words: &[Word]) -> Self {
        // Unregistered guard words prevent Boehm merging adjacent root ranges.
        let mut storage = vec![0; words.len() + 2];
        storage[1..words.len() + 1].copy_from_slice(words);
        let words = storage
            .into_iter()
            .map(std::cell::UnsafeCell::new)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        if !copying::enabled() {
            rt_init();
        }
        if words.len() > 2 {
            // SAFETY: this stable, exclusively owned allocation lasts until Drop removes it.
            unsafe {
                register(
                    words.as_ptr().add(1) as *mut Word,
                    words.len() - 2,
                );
            }
        }
        Self {
            words,
            thread: std::marker::PhantomData,
        }
    }
    /// Copy one current word without lending a reference that relocation could invalidate.
    pub fn get(&self, index: usize) -> Word {
        assert!(index < self.len());
        // SAFETY: the initialized cell is read between collections on its owning thread.
        unsafe { self.words[index + 1].get().read() }
    }
    pub fn len(&self) -> usize {
        self.words.len() - 2
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    /// Raw ABI storage, never a Rust reference to collector-mutable words.
    pub fn as_ptr(&self) -> *const Word {
        self.words.as_ptr().wrapping_add(1).cast()
    }
    pub fn snapshot(&self) -> Vec<Word> {
        (0..self.len()).map(|i| self.get(i)).collect()
    }
}

impl Drop for Roots {
    fn drop(&mut self) {
        if self.words.len() > 2 {
            // SAFETY: removes exactly the registered range, before its allocation is freed.
            unsafe {
                unregister(
                    self.words.as_ptr().add(1) as *mut Word,
                    self.words.len() - 2,
                );
            }
        }
    }
}

/// Force a collection for ABI validation. Live words must be on registered stacks or roots.
pub fn collect() {
    if copying::enabled() {
        copying::collect();
        return;
    }
    #[cfg(feature = "copying-only")]
    panic!("copying collector must be initialized before allocation");
    #[cfg(not(feature = "copying-only"))]
    {
        rt_init();
        // SAFETY: the calling thread is registered and the collector owns its heap.
        unsafe {
            GC_gcollect();
        }
    }
}

/// Allocate a scanned object, protecting fields even when supplied in a Rust Vec.
pub fn object(kind: u8, tag: u32, fields: &[Word]) -> Word {
    let is_value = |i: usize| match kind {
        layout::CLOSURE => i >= layout::CLOSURE_CAPTURES,
        layout::PAP => i != layout::PAP_COUNT,
        layout::CON => true,
        _ => panic!("scanned allocation requires a scanned object kind"),
    };
    let values = fields
        .iter()
        .enumerate()
        .filter_map(|(i, w)| is_value(i).then_some(*w))
        .collect::<Vec<_>>();
    let roots = Roots::new(&values);
    let size = u32::try_from(fields.len()).expect("object too large");
    let p = rt_alloc(fields.len() + 1);
    let mut next = 0;
    // SAFETY: p has room for the header and fields. Only tagged values can relocate.
    unsafe {
        p.write(layout::header(kind, tag, size));
        for (i, word) in fields.iter().enumerate() {
            let value = if is_value(i) {
                let v = roots.get(next);
                next += 1;
                v
            } else {
                *word
            };
            p.add(i + 1).write(value);
        }
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
/// w must be a live STRING. No collection may occur during the returned borrow.
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

/// A persistent global written by generated code. UnsafeCell permits those writes
/// through a stable raw address without creating competing mutable Rust borrows.
/// Guard cells keep independently removable Boehm root ranges disjoint.
pub struct GlobalRoot {
    words: Box<[std::cell::UnsafeCell<Word>; 3]>,
    thread: std::marker::PhantomData<std::rc::Rc<()>>,
}

impl GlobalRoot {
    pub fn new(value: Word) -> Self {
        if !copying::enabled() {
            rt_init();
        }
        let words = Box::new([
            std::cell::UnsafeCell::new(0),
            std::cell::UnsafeCell::new(value),
            std::cell::UnsafeCell::new(0),
        ]);
        // SAFETY: cells are contiguous, stable, and owned until Drop; only the middle
        // word is registered. Generated code and the collector run on registered threads.
        unsafe {
            register(words[1].get(), 1);
        }
        Self {
            words,
            thread: std::marker::PhantomData,
        }
    }
    pub fn as_ptr(&self) -> *mut Word {
        self.words[1].get()
    }
    /// Read between executions. This owner is not Sync, so Rust cannot share it across threads.
    pub fn get(&self) -> Word {
        // SAFETY: the slot is initialized and no generated call is executing concurrently.
        unsafe { self.words[1].get().read() }
    }
}

impl Drop for GlobalRoot {
    fn drop(&mut self) {
        // SAFETY: unregister exactly this live range before freeing its backing allocation.
        unsafe {
            unregister(self.words[1].get(), 1);
        }
    }
}

unsafe fn register(p: *mut Word, len: usize) {
    // SAFETY: callers keep the registered range alive until unregister.
    unsafe {
        if copying::enabled() {
            copying::register(p, len);
        } else {
            #[cfg(not(feature = "copying-only"))]
            GC_add_roots(p.cast(), p.add(len).cast());
            #[cfg(feature = "copying-only")]
            panic!(
                "copying collector must be initialized before registering roots"
            );
        }
    }
}

unsafe fn unregister(p: *mut Word, len: usize) {
    if copying::enabled() {
        copying::unregister(p);
    } else {
        #[cfg(not(feature = "copying-only"))]
        // SAFETY: this range was registered by the same owner.
        unsafe {
            GC_remove_roots(p.cast(), p.add(len).cast());
        }
        #[cfg(feature = "copying-only")]
        {
            let _ = len;
            panic!("copying collector is not initialized");
        }
    }
}
