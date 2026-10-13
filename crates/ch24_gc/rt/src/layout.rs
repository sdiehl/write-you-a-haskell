//! The ABI shared by the compiler and runtime. Sizes count payload words.
pub type Word = u64;

pub const WORD_BYTES: usize = 8;

pub const INT_MIN: i64 = -(1_i64 << 62);

pub const INT_MAX: i64 = (1_i64 << 62) - 1;

pub const CON: u8 = 0;

pub const CLOSURE: u8 = 1;

pub const PAP: u8 = 2;

pub const DOUBLE: u8 = 3;

pub const STRING: u8 = 4;

pub const FORWARD: u8 = 5;

pub const FALSE: Word = immediate(0);

pub const TRUE: Word = immediate(1);

pub const NIL: Word = immediate(0);

pub const UNIT: Word = immediate(0);

pub const NOTHING: Word = immediate(0);

pub const LT: Word = immediate(0);

pub const EQ: Word = immediate(1);

pub const GT: Word = immediate(2);

pub const CONS_TAG: u32 = 1;

pub const JUST_TAG: u32 = 1;

pub const CLOSURE_CODE: usize = 0;

pub const CLOSURE_ARITY: usize = 1;

pub const CLOSURE_CAPTURES: usize = 2;

pub const PAP_CLOSURE: usize = 0;

pub const PAP_COUNT: usize = 1;

pub const PAP_ARGS: usize = 2;

pub const fn normalize(n: i64) -> i64 {
    (n << 1) >> 1
}

pub const fn integer(n: i64) -> Word {
    ((n as u64) << 1) | 1
}

pub const fn signed(w: Word) -> i64 {
    (w as i64) >> 1
}

pub const fn immediate(tag: u32) -> Word {
    ((tag as Word) << 1) | 1
}

pub const fn is_immediate(w: Word) -> bool {
    w & 1 != 0
}

/// Bits 0..8: kind, 8..32: tag, 32..64: payload word count.
pub const fn header(kind: u8, tag: u32, size: u32) -> Word {
    assert!(tag <= 0x00ff_ffff);
    kind as Word | ((tag as Word) << 8) | ((size as Word) << 32)
}

pub const fn kind(h: Word) -> u8 {
    h as u8
}

pub const fn tag(h: Word) -> u32 {
    ((h >> 8) & 0x00ff_ffff) as u32
}

pub const fn size(h: Word) -> usize {
    (h >> 32) as usize
}
