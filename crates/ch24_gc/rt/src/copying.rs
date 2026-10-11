//! Cheney collection with exact object fields and explicitly registered root slots.
//! Collection is single-threaded. Native code selects this heap before allocating.
use crate::layout::{self, Word};
use std::{cell::RefCell, collections::BTreeMap, ptr};

#[derive(Default)]
struct Heap {
    enabled: bool,
    from: Vec<Word>,
    spare: Vec<Word>,
    used: usize,
    roots: BTreeMap<usize, usize>,
    frames: Vec<usize>,
    stress: bool,
    collections: usize,
    copied_words: usize,
}
thread_local! { static HEAP: RefCell<Heap> = RefCell::new(Heap::default()); }
/// Select the precise heap once, before program initialization.
#[unsafe(no_mangle)]
pub extern "C" fn rt_use_copying() {
    crate::primitives::init_io();
    HEAP.with_borrow_mut(|h| {
        assert!(!h.enabled && h.roots.is_empty());
        h.enabled = true;
        h.stress = std::env::var("FUN_GC_STRESS").is_ok_and(|s| s == "1");
        h.from = vec![0; 8192];
        h.spare = vec![0; 8192];
    });
}
pub fn enabled() -> bool {
    HEAP.with_borrow(|h| h.enabled)
}
/// # Safety
/// The range must be writable, initialized tagged words until unregistered.
pub unsafe fn register(p: *mut Word, len: usize) {
    HEAP.with_borrow_mut(|h| {
        assert!(h.roots.insert(p as usize, len).is_none());
    });
}
pub fn unregister(p: *mut Word) {
    HEAP.with_borrow_mut(|h| {
        assert!(h.roots.remove(&(p as usize)).is_some());
    });
}
/// # Safety
/// Generated code supplies a distinct stable stack array, initialized to zero.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rt_roots_push(p: *mut Word, len: usize) {
    // SAFETY: the generated frame obeys the registration contract.
    unsafe {
        register(p, len);
    }
    HEAP.with_borrow_mut(|h| h.frames.push(p as usize));
}
/// Remove the current frame before return or a known tail transfer.
#[unsafe(no_mangle)]
pub extern "C" fn rt_roots_pop() {
    HEAP.with_borrow_mut(|h| {
        let p = h.frames.pop().expect("shadow stack underflow");
        assert!(h.roots.remove(&p).is_some());
    });
}
/// # Safety
/// p is a static writable word, live until process exit.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rt_global_root(p: *mut Word) {
    // SAFETY: native startup provides stable global storage.
    unsafe {
        register(p, 1);
    }
}
// ANCHOR: evacuate
fn evacuate(
    from: &mut [Word],
    used: usize,
    to: &mut [Word],
    free: &mut usize,
    w: Word,
) -> Word {
    let base = from.as_ptr() as usize;
    let address = w as usize;
    // Immediates, null placeholders, and static string/code addresses never move.
    if layout::is_immediate(w)
        || address < base
        || address >= base + used * 8
    {
        return w;
    }
    assert_eq!((address - base) % 8, 0);
    let start = (address - base) / 8;
    let header = from[start];
    if layout::kind(header) == layout::FORWARD {
        return from[start + 1];
    }
    let words = 1 + layout::size(header);
    assert!(words >= 2 && start + words <= used);
    let dest = *free;
    to[dest..dest + words].copy_from_slice(&from[start..start + words]);
    *free += words;
    let pointer = to.as_mut_ptr().wrapping_add(dest) as Word;
    from[start] = layout::header(layout::FORWARD, 0, 1);
    from[start + 1] = pointer;
    pointer
}
// ANCHOR_END: evacuate
impl Heap {
    // ANCHOR: cheney
    fn collect(&mut self, reserve: usize) {
        let capacity = if self.used + reserve > self.from.len() {
            self.from
                .len()
                .saturating_mul(2)
                .max(self.used.checked_add(reserve).expect("heap overflow"))
        } else {
            self.from.len()
        };
        self.spare.resize(capacity, 0);
        let mut free = 0;
        for (&address, &len) in &self.roots {
            for i in 0..len {
                let slot = (address as *mut Word).wrapping_add(i);
                // SAFETY: registered ranges remain writable throughout collection.
                unsafe {
                    slot.write(evacuate(
                        &mut self.from,
                        self.used,
                        &mut self.spare,
                        &mut free,
                        slot.read(),
                    ));
                }
            }
        }
        let mut scan = 0;
        while scan < free {
            let header = self.spare[scan];
            let size = layout::size(header);
            let first = match layout::kind(header) {
                layout::CON => 0,
                layout::CLOSURE => 2, // code and arity are raw, not values
                layout::PAP => 0,
                layout::DOUBLE | layout::STRING => size,
                _ => panic!("invalid object in to-space"),
            };
            for field in first..size {
                if layout::kind(header) == layout::PAP && field == 1 {
                    continue;
                }
                let slot = scan + 1 + field;
                let value = self.spare[slot];
                self.spare[slot] = evacuate(
                    &mut self.from,
                    self.used,
                    &mut self.spare,
                    &mut free,
                    value,
                );
            }
            scan += size + 1;
        }
        std::mem::swap(&mut self.from, &mut self.spare);
        self.used = free;
        self.collections += 1;
        self.copied_words += free;
    }
    // ANCHOR_END: cheney
}
pub fn allocate(words: usize) -> *mut Word {
    HEAP.with_borrow_mut(|h| {
        assert!(h.enabled);
        let required =
            h.used.checked_add(words).expect("heap size overflow");
        if h.stress || required > h.from.len() {
            h.collect(0);
        }
        // Leave at least as much free space as live data after a collection.
        // Otherwise a nearly full live heap repeatedly copies itself for a few words.
        if required > h.from.len()
            && h.used.saturating_mul(2) + words > h.from.len()
        {
            let reserve = h.from.len().saturating_sub(h.used) + words;
            h.collect(reserve);
        } else if h.used + words > h.from.len() {
            h.collect(words);
        }
        let p = h.from.as_mut_ptr().wrapping_add(h.used);
        h.used = h.used.checked_add(words).expect("heap size overflow");
        // SAFETY: the reserved range is within from-space and is not live yet.
        unsafe {
            ptr::write_bytes(p, 0, words);
        }
        p
    })
}
pub fn collect() {
    HEAP.with_borrow_mut(|h| h.collect(0));
}
/// Observable counters for collector contract tests, not source primitives.
pub fn statistics() -> (usize, usize, usize) {
    HEAP.with_borrow(|h| (h.collections, h.used, h.frames.len()))
}

/// Current semispace capacity, used to test bounded transient allocation.
pub fn capacity_words() -> usize {
    HEAP.with_borrow(|h| h.from.len())
}

/// Total words copied, including growth relocations.
pub fn copied_words() -> usize {
    HEAP.with_borrow(|h| h.copied_words)
}
