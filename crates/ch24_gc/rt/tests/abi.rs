//! This harness owns main, so GC initialization precedes any test-created threads.
use fun_rt_24::{
    self as rt,
    abi::*,
    apply::{self, Slow},
    heap::{self, Roots},
    layout::{self as l, Word},
};
use std::process::{Command, Stdio};
unsafe extern "C" fn identity(_: Word, args: *const Word) -> Word {
    // SAFETY: this entry has exactly one argument.
    unsafe { *args }
}
unsafe extern "C" fn sum3(_: Word, args: *const Word) -> Word {
    // SAFETY: the closure's declared arity is three, and apply supplies three live words.
    unsafe {
        l::integer(
            l::signed(*args)
                + l::signed(*args.add(1))
                + l::signed(*args.add(2)),
        )
    }
}
unsafe extern "C" fn add_capture(env: Word, args: *const Word) -> Word {
    heap::collect();
    // SAFETY: this closure has one capture and two arguments, as constructed below.
    unsafe {
        l::integer(
            l::signed(heap::field(env, 2))
                + l::signed(*args)
                + l::signed(*args.add(1)),
        )
    }
}
unsafe extern "C" fn make_adder(_: Word, args: *const Word) -> Word {
    // SAFETY: arity one guarantees the captured argument exists.
    unsafe { apply::closure(add_capture, 2, &[*args]) }
}
unsafe extern "C" fn parity(env: Word, args: *const Word) -> Word {
    heap::collect();
    // SAFETY: a parity closure has the partner closure and base result as its two captures.
    unsafe {
        let n = l::signed(*args);
        if n == 0 {
            heap::field(env, 3)
        } else {
            rt::rt_apply1(heap::field(env, 2), l::integer(n - 1))
        }
    }
}
fn main() {
    rt::rt_init();
    let args: Vec<String> = std::env::args().skip(1).collect();
    // Test runners list a harness before running it; this one holds a single test.
    if args.first().is_some_and(|flag| flag == "--list") {
        if !args.iter().any(|flag| flag == "--ignored") {
            println!("abi: test");
        }
        return;
    }
    if let Some(mode) = args.first().filter(|mode| !mode.starts_with("--"))
    {
        print!("before");
        // SAFETY: all test operands are constructed with the runtime's own typed allocators.
        unsafe {
            match mode.as_str() {
                "zero" => {
                    rt_prim_intDiv(l::integer(1), l::integer(0));
                }
                "error" => {
                    rt_prim_error(heap::string("failure"));
                }
                "chr" => {
                    rt_prim_chrChr(l::integer(0xd800));
                }
                "pattern" => {
                    rt::rt_pattern_fail(c"test.fun".as_ptr(), 2, 7)
                }
                "exit" => {
                    rt_prim_exit(l::integer(7));
                }
                "input" => {
                    let line = rt_prim_getLine(l::UNIT);
                    rt_prim_putStr(line);
                }
                _ => panic!("unknown child mode"),
            }
        }
        rt::rt_exit(0);
    }
    // SAFETY: every below raw read and application uses a live, rooted object and known layout.
    unsafe {
        for n in [l::INT_MIN, -1, 0, 1, l::INT_MAX] {
            assert_eq!(l::signed(l::integer(n)), n);
        }
        assert_eq!(l::signed(l::integer(l::INT_MAX + 1)), l::INT_MIN);
        for kind in 0..=5 {
            let h = l::header(kind, 0xabcdef, 4096);
            assert_eq!(
                (l::kind(h), l::tag(h), l::size(h)),
                (kind, 0xabcdef, 4096)
            );
        }
        assert_eq!(
            (
                l::FALSE,
                l::TRUE,
                l::NIL,
                l::UNIT,
                l::NOTHING,
                l::LT,
                l::EQ,
                l::GT
            ),
            (1, 3, 1, 1, 1, 1, 3, 5)
        );
        let big = heap::constructor(
            123,
            &(0..4096).map(l::integer).collect::<Vec<_>>(),
        );
        let roots = Roots::new(&[big]);
        heap::collect();
        assert_eq!(l::tag(heap::header(roots.get(0))), 123);
        for i in 0..4096 {
            assert_eq!(l::signed(heap::field(big, i)), i as i64);
        }
        let f = apply::closure(sum3 as Slow, 3, &[]);
        let pap = rt::rt_apply1(f, l::integer(10));
        assert_eq!(l::kind(heap::header(pap)), l::PAP);
        let roots = Roots::new(&[pap]);
        heap::collect();
        let pap2 = rt::rt_apply1(roots.get(0), l::integer(20));
        assert_eq!(l::signed(rt::rt_apply1(pap2, l::integer(12))), 42);
        assert_eq!(
            l::signed(rt::rt_apply3(
                f,
                l::integer(10),
                l::integer(20),
                l::integer(12)
            )),
            42
        );
        assert_eq!(
            l::signed(rt::rt_apply2(pap, l::integer(20), l::integer(12))),
            42
        );
        let zero = rt::rt_apply(f, 0, std::ptr::null());
        assert_eq!(
            l::signed(rt::rt_apply3(
                zero,
                l::integer(10),
                l::integer(20),
                l::integer(12)
            )),
            42
        );
        let maker = apply::closure(make_adder, 1, &[]);
        assert_eq!(
            l::signed(rt::rt_apply3(
                maker,
                l::integer(10),
                l::integer(20),
                l::integer(12)
            )),
            42
        );
        let id = apply::closure(identity, 1, &[]);
        assert_eq!(
            l::signed(rt::rt_apply4(
                id,
                maker,
                l::integer(10),
                l::integer(20),
                l::integer(12)
            )),
            42
        );
        let even = apply::closure(parity, 1, &[l::UNIT, l::TRUE]);
        let odd = apply::closure(parity, 1, &[l::UNIT, l::FALSE]);
        rt::rt_closure_set(even, 0, odd);
        rt::rt_closure_set(odd, 0, even);
        let roots = Roots::new(&[even]);
        heap::collect();
        assert_eq!(rt::rt_apply1(roots.get(0), l::integer(12)), l::TRUE);
        assert_eq!(rt::rt_apply1(roots.get(0), l::integer(11)), l::FALSE);
        let s = heap::string("é中🦀\0\n");
        assert_eq!(heap::field(s, 0), 11);
        assert_eq!(l::signed(rt_prim_strLength(s)), 5);
        let list = rt_prim_strUnpack(s);
        assert_eq!(heap::text(rt_prim_strPack(list)), "é中🦀\0\n");
        assert_eq!(
            heap::text(rt_prim_strAppend(s, heap::string("λ"))),
            "é中🦀\0\nλ"
        );
        assert_eq!(rt_prim_strEq(s, heap::string("é中🦀\0\n")), l::TRUE);
        assert_eq!(
            l::signed(rt_prim_strCompare(
                heap::string("a"),
                heap::string("é")
            )),
            -1
        );
        assert_eq!(
            heap::text(rt_prim_showChar(l::integer('\n' as i64))),
            "'\\n'"
        );
        assert_eq!(heap::text(rt_prim_showString(s)), "\"é中🦀\\0\\n\"");
        assert_eq!(
            rt_prim_readInt(heap::string("4611686018427387904")),
            l::NOTHING
        );
        let parsed = rt_prim_readInt(heap::string(" -42 "));
        assert_eq!(l::tag(heap::header(parsed)), l::JUST_TAG);
        assert_eq!(l::signed(heap::field(parsed, 0)), -42);
        assert_eq!(
            l::signed(rt_prim_intDiv(l::integer(-7), l::integer(3))),
            -3
        );
        assert_eq!(
            l::signed(rt_prim_intMod(l::integer(-7), l::integer(3))),
            2
        );
        assert_eq!(
            l::signed(rt_prim_intQuot(l::integer(-7), l::integer(3))),
            -2
        );
        assert_eq!(
            l::signed(rt_prim_intRem(l::integer(-7), l::integer(3))),
            -1
        );
        assert_eq!(
            l::signed(rt_prim_intMul(
                l::integer(l::INT_MAX),
                l::integer(2)
            )),
            -2
        );
        assert_eq!(l::signed(rt_prim_dblRound(heap::double(2.5))), 2);
        assert_eq!(l::signed(rt_prim_dblRound(heap::double(3.5))), 4);
        assert_eq!(l::signed(rt_prim_dblFloor(heap::double(f64::NAN))), 0);
        assert_eq!(
            l::signed(rt_prim_dblTruncate(heap::double(f64::INFINITY))),
            l::INT_MAX
        );
        assert_eq!(
            heap::unbox_double(rt_prim_dblSqrt(heap::double(25.0))),
            5.0
        );
        for line in include_str!("double.tsv").lines() {
            let (input, expected) = line.split_once('\t').unwrap();
            let n: f64 = input.parse().unwrap();
            assert_eq!(
                heap::text(rt_prim_showDouble(heap::double(n))),
                expected,
                "{input}"
            );
        }
        let roots = Roots::new(&[heap::string("survives allocations")]);
        for i in 0..10000 {
            let _ = heap::constructor(0, &[l::integer(i); 128]);
        }
        heap::collect();
        assert_eq!(heap::text(roots.get(0)), "survives allocations");
    }
    std::thread::spawn(|| {
        let roots = Roots::new(&[heap::string("worker")]);
        heap::collect();
        // SAFETY: the string stays in a registered root while its contents are inspected.
        assert_eq!(unsafe { heap::text(roots.get(0)) }, "worker");
    })
    .join()
    .unwrap();
    for (mode, code, error) in [
        ("zero", 1, "fun: divide by zero\n"),
        ("error", 1, "fun: failure\n"),
        ("chr", 1, "fun: invalid character code point\n"),
        ("pattern", 1, "fun: pattern match failure at test.fun:2:7\n"),
        ("exit", 7, ""),
    ] {
        let out = Command::new(std::env::current_exe().unwrap())
            .arg(mode)
            .output()
            .unwrap();
        assert_eq!(out.status.code(), Some(code), "{mode}");
        assert_eq!(out.stdout, b"before");
        assert_eq!(out.stderr, error.as_bytes());
    }
    let mut child = Command::new(std::env::current_exe().unwrap())
        .arg("input")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    use std::io::Write;
    child
        .stdin
        .take()
        .unwrap()
        .write_all("你好\n".as_bytes())
        .unwrap();
    let out = child.wait_with_output().unwrap();
    assert!(out.status.success());
    assert_eq!(out.stdout, "before你好".as_bytes());
    assert!(out.stderr.is_empty());
    let out = Command::new(std::env::current_exe().unwrap())
        .arg("input")
        .stdin(Stdio::null())
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(1));
    assert_eq!(out.stdout, b"before");
    assert_eq!(out.stderr, b"fun: end of input\n");
    println!(
        "runtime ABI: tagging, objects, apply/PAP, letrec, GC, 200 doubles, Unicode and IO passed"
    );
}
