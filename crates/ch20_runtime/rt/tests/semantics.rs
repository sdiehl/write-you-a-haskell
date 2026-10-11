use fun_rt_20::{self as rt, Prim, Value};
#[test]
fn two_hundred_double_formats_and_round_trips() {
    let cases = include_str!("double.tsv").lines().collect::<Vec<_>>();
    assert_eq!(cases.len(), 200);
    for line in cases {
        let (input, expected) = line.split_once('\t').unwrap();
        let n: f64 = input.parse().unwrap();
        let actual = rt::show_double(n);
        assert_eq!(actual, expected, "{input}");
        if n.is_finite() {
            assert_eq!(
                actual.parse::<f64>().unwrap().to_bits(),
                n.to_bits(),
                "{input}"
            );
        }
    }
}
#[test]
fn read_int_and_unicode_boundaries() {
    for s in [
        "",
        "+",
        "-",
        "1_0",
        "0xff",
        "1.0",
        "12x",
        "٤",
        "4611686018427387904",
        "-4611686018427387905",
    ] {
        assert_eq!(rt::read_int(s), None, "{s}");
    }
    assert_eq!(rt::read_int(" +42 "), Some(42));
    assert_eq!(
        rt::read_int("-4611686018427387904"),
        Some(rt::layout::INT_MIN)
    );
    assert_eq!(
        rt::quote("\0\n\t\r\\\"中", '"'),
        "\"\\0\\n\\t\\u{d}\\\\\\\"中\""
    );
    assert_eq!(
        rt::evaluate(Prim::StrLength, &[Value::String("é中🦀".into())]),
        Ok(Value::Int(3))
    );
    assert_eq!(
        rt::evaluate(Prim::ChrChr, &[Value::Int(0x10ffff)]),
        Ok(Value::Char('\u{10ffff}'))
    );
    for n in [-1, 0xd800, 0x110000] {
        assert_eq!(
            rt::evaluate(Prim::ChrChr, &[Value::Int(n)]),
            Err(rt::Failure::Runtime(
                "invalid character code point".into()
            ))
        );
    }
}
#[test]
fn signed_division_and_saturating_conversions() {
    for (a, b, q, r, d, m) in [
        (7, 3, 2, 1, 2, 1),
        (-7, 3, -2, -1, -3, 2),
        (7, -3, -2, 1, -3, -2),
        (-7, -3, 2, -1, 2, -1),
        (
            rt::layout::INT_MIN,
            -1,
            rt::layout::INT_MIN,
            0,
            rt::layout::INT_MIN,
            0,
        ),
    ] {
        for (op, n) in [
            (Prim::IntQuot, q),
            (Prim::IntRem, r),
            (Prim::IntDiv, d),
            (Prim::IntMod, m),
        ] {
            assert_eq!(
                rt::evaluate(op, &[Value::Int(a), Value::Int(b)]),
                Ok(Value::Int(n))
            );
        }
    }
    for (n, expected) in [
        (f64::NAN, 0),
        (f64::INFINITY, rt::layout::INT_MAX),
        (f64::NEG_INFINITY, rt::layout::INT_MIN),
        (2.5, 2),
        (3.5, 4),
        (-2.5, -2),
        (-3.5, -4),
    ] {
        assert_eq!(
            rt::evaluate(Prim::DblRound, &[Value::Double(n)]),
            Ok(Value::Int(expected))
        );
    }
}
