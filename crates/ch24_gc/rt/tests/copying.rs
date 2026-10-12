use fun_rt_24::{
    copying,
    heap::{self, GlobalRoot, Roots},
    layout,
};
fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    // Test runners list a harness before running it; this one holds a single test.
    if args.first().is_some_and(|flag| flag == "--list") {
        if !args.iter().any(|flag| flag == "--ignored") {
            println!("copying: test");
        }
        return;
    }
    copying::rt_use_copying();
    let leaf = heap::string("moving λ");
    let shared = heap::constructor(0, &[leaf, leaf]);
    let root = Roots::new(&[shared]);
    let old = root.get(0);
    copying::collect();
    let moved = root.get(0);
    assert_ne!(old, moved);
    // SAFETY: the root owns a live two-field constructor through these reads.
    unsafe {
        assert_eq!(heap::field(moved, 0), heap::field(moved, 1));
        assert_eq!(heap::text(heap::field(moved, 0)), "moving λ");
    }
    let cycle = GlobalRoot::new(heap::constructor(1, &[layout::UNIT]));
    // SAFETY: unpublished constructor's single slot is exclusively patched here.
    unsafe {
        (cycle.get() as *mut u64).add(1).write(cycle.get());
    }
    copying::collect();
    // SAFETY: cycle is registered and retains the self-edge after relocation.
    unsafe {
        assert_eq!(heap::field(cycle.get(), 0), cycle.get());
    }
    // A bit pattern that happens to equal a heap address must not be traced in a Double.
    let bits = cycle.get();
    let number = GlobalRoot::new(heap::double(f64::from_bits(bits)));
    copying::collect();
    // SAFETY: number is a rooted Double object.
    unsafe {
        assert_eq!(heap::unbox_double(number.get()).to_bits(), bits);
    }
    let list = GlobalRoot::new(layout::NIL);
    for i in 0..100_000 {
        let next = heap::constructor(1, &[layout::integer(i), list.get()]);
        // SAFETY: this owner updates its stable root only between heap operations.
        unsafe {
            list.as_ptr().write(next);
        }
    }
    copying::collect();
    let mut p = list.get();
    for i in (0..100_000).rev() {
        // SAFETY: no allocation occurs during the walk through rooted list fields.
        unsafe {
            assert_eq!(layout::signed(heap::field(p, 0)), i);
            p = heap::field(p, 1);
        }
    }
    assert_eq!(p, layout::NIL);
    // Keep seven eighths of the current space live, then churn dead objects.
    // A collector that grows only on allocation failure repeatedly copies this
    // same live graph. Measure copying work, rather than wall-clock timing.
    copying::collect();
    let capacity = copying::capacity_words();
    let target = capacity * 7 / 8;
    while copying::statistics().1 + 3 < target {
        let next = heap::constructor(1, &[layout::integer(0), list.get()]);
        // SAFETY: the stable root is updated between allocations.
        unsafe {
            list.as_ptr().write(next);
        }
    }
    let before = copying::copied_words();
    for _ in 0..capacity {
        heap::constructor(1, &[layout::integer(0)]);
    }
    assert!(copying::copied_words() - before < capacity * 8);
    assert!(copying::capacity_words() > capacity);
    drop(list);
    drop(number);
    drop(cycle);
    drop(root);
    copying::collect();
    let (collections, words, frames) = copying::statistics();
    assert!(collections > 5);
    assert_eq!((words, frames), (0, 0));
    println!(
        "copying: relocation, sharing, cycles, atomic payloads, growth and reclamation passed"
    );
}
