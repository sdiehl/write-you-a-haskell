# Chapter 20: Data Representation and the Runtime

> **New files**
>
> - `rt/src/layout.rs`
> - `rt/src/abi.rs`
> - `rt/src/heap.rs`
> - `rt/src/apply.rs`
> - `rt/src/primitives.rs`
> - `rt/build.rs`
> - `rt/src/lib.rs`
>
> **Changed files**
>
> - `src/primitives.rs`
> - `src/lib.rs`
> - `src/main.rs`

## Motivation

Our interpreters have enjoyed a useful luxury. Rust enums describe their
values, and Rust helps manage their storage. The machine code we're about to
generate won't have those enums. We need to decide exactly what an integer,
a list node, or a closure looks like in memory.

Polymorphism makes the question especially interesting. One compiled `id`
function should accept an integer, a string, or a tree. We'd like to pass all
of them through the same calling convention. A uniform machine word gives us
that common representation, with a little tagging to distinguish immediate
values from pointers.

We'll build the runtime before connecting LLVM. This lets us inspect the memory
layout and application protocol on their own. The interpreters will also reuse
its primitive operations. We shouldn't need three slightly different ideas of
how to print a floating-point number.

## The Language, Now

The programs we write don't change here. Our new representation must still
give us wrapping 63-bit integers, Unicode characters, UTF-8 strings, and strict
constructor fields. Changing how a value fits in memory shouldn't change what
a Fun program can observe.

This inherited program exercises floating display for NaN, infinity, and signed
zero:

```fun
{{#include ../../crates/ch20_runtime/tests/programs/classes/float-special.fun}}
```

The runtime is available as `fun-rt-20`, built both as an `rlib` for the compiler
and a `staticlib` for external linking. The chapter compiler still runs Core,
ANF, or Flat. Linking the runtime into a C test does not yet provide `fun build`.
LLVM object generation is the next chapter's task.

## Theory

A value is a 64-bit word. An immediate has low bit one. An aligned pointer has
low bit zero. Encoding an integer shifts it left one bit and sets the tag, leaving
63 signed bits. Decoding uses an arithmetic shift. The same immediate encoding
holds character scalar values and nullary constructor tags.

```rust
#![function!("../../crates/ch20_runtime/rt/src/layout.rs", integer)]
#![function!("../../crates/ch20_runtime/rt/src/layout.rs", signed)]
```

Notice something odd about word `1`. It can represent unit, `False`, the empty
list, or `Nothing`. That isn't an ambiguity the runtime has to resolve. The
type checker already established which interpretation the program can use.
We don't inspect a list as though it were a boolean.

This is what makes the uniform representation useful. A polymorphic function
can pass words around without needing a different memory layout for every
instantiation. Tagging and boxing give strict functional implementations a
practical way to make that work [@leroy1990].

Heap objects begin with a header. Its low eight bits identify the object kind,
the next 24 bits carry a constructor tag, and the high 32 bits count payload
words. Constructor tags are local to a datatype and follow declaration order.
The fixed Prelude tags keep runtime-created lists and optional values compatible
with compiler-created ones.

Boehm finds potential pointers by inspecting words in memory [@boehm1988].
That saves us from describing every live pointer in the generated code. The
tradeoff is that an unrelated bit pattern can look like an address and keep
an otherwise dead object alive. The collector also can't freely move an object
when it doesn't know which apparent pointers are real references to update.

A precise moving collector needs our help. We'll build that cooperation in
Chapter 24, using an explicit account of roots. Henderson and the GC handbook
develop this distinction [@henderson2002; @gc2023].

## Implementation

`rt/src/layout.rs` is the shared ABI definition, re-exported by the compiler as
`runtime_layout` to avoid its existing offside-layout module. Header construction
checks that the constructor tag fits:

```rust
#![function!("../../crates/ch20_runtime/rt/src/layout.rs", header)]
```

The payload layouts are deliberately simple:

| Kind      | Payload after the header                              |
| --------- | ----------------------------------------------------- |
| `CON`     | Constructor fields, one word each                     |
| `CLOSURE` | Slow code address, arity, then captures               |
| `PAP`     | Closure word, argument count, then supplied arguments |
| `DOUBLE`  | One word containing binary64 bits                     |
| `STRING`  | Byte length, then padded UTF-8 bytes                  |
| `FORWARD` | Reserved for the later copying collector              |

### Reading the Heap

Start with the word held in a variable. Its low bit tells us whether the value
fits in that word or points to an object. A pointer addresses the **header**, not
the first field. All offsets below are bytes, and every row occupies eight bytes.

```text
Value word (64 bits)

Int 42:     [ 85 ]  low bit 1
Heap value: [ p  ]  low bit 0
              |
              v
+----------------------------+
| header                     | +0
| payload[0]                 | +8
| payload[1]                 | +16
| ...                        |
+----------------------------+

Header bits (most significant first)
 63             32 31           8 7            0
+-----------------+--------------+--------------+
| size: 32 bits   | tag: 24 bits | kind: 8 bits |
+-----------------+--------------+--------------+
```

For example, `Just 42` needs one payload word. Its constructor tag is `1`.
A `Double` also needs one payload word, but that word contains floating-point
bits rather than an encoded Fun value.

```text
Just 42
+----------------------------+
| CON, tag=1, size=1         | +0
| 85 (tagged Int 42)         | +8
+----------------------------+

Double 3.0
+----------------------------+
| DOUBLE, tag=0, size=1      | +0
| 0x4008000000000000         | +8
+----------------------------+
```

A closure packages code with the values it captured. Imagine the local function
`f x y = k + x + y`, with `k = 10`. Its arity is two and its environment has one
capture. Applying `f` to just `20` creates a partial application, or PAP, which
points back to the closure and remembers that argument.

```text
PAP for f 20
+----------------------------+
| PAP, tag=0, size=3         | +0
| closure word               | +8 ----+
| argument count: 1          | +16    |
| 41 (tagged Int 20)         | +24    |
+----------------------------+        |
       +------------------------------+
       |
       |  Closure for f
       |  +----------------------------+
       +->| CLOSURE, tag=0, size=3     | +0
          | slow entry address         | +8
          | arity: 2                   | +16
          | capture k: 21 (Int 10)     | +24
          +----------------------------+
```

The arrow denotes a pointer to the whole closure object. Arity and argument
count are raw counts, not tagged integers. Captures and saved arguments are Fun
value words, so they may themselves point to other objects. The slow entry is a
code address. Calling the PAP with `22` combines the saved `20` with `22`, then
enters the closure with both arguments. The closure's capture remains `10`.

A string stores a byte count followed by UTF-8 bytes padded to whole words.
For `"hi"`, the payload has two words: the count and one word of byte storage.
The padding is not part of the string, and no terminating zero is required.

```text
String "hi"
+----------------------------+
| STRING, tag=0, size=2      | +0
| byte length: 2             | +8
| 68 69 .. .. .. .. .. ..    | +16
+----------------------------+
Hex bytes; .. denotes padding.
```

`rt_alloc` takes a total word count including the header. `rt_alloc_atomic`
takes bytes and asks Boehm not to scan the contents. Doubles and strings use
atomic allocation because their payload contains no value pointers. A string's
stored length counts bytes. The source string-length primitive counts Unicode
scalars. Alignment is eight bytes throughout.

Allocation must protect input fields while the collector runs. A Rust `Vec` is
allocated by Rust's allocator, and Boehm does not automatically trace through
that buffer. `Roots` owns a stable boxed word buffer and registers its interior
range with the collector. Its destructor removes the range before freeing the
buffer. Two unregistered guard words keep adjacent registrations distinct.
Object construction uses this protection before allocating and copying fields:

```rust
#![function!("../../crates/ch20_runtime/rt/src/heap.rs", object)]
```

The runtime initializes Boehm on the main thread. Foreign Rust worker threads
register their stacks and unregister when they exit. Direct ABI tests force
collection inside calls and while cyclic closure environments are live. Those
cases would expose assumptions that work only when allocation happens not to
collect.

A closure stores a C-convention slow entry and its syntactic arity. The entry
receives the closure word and a pointer to exactly that arity's arguments.
Closure construction simply builds the corresponding object:

```rust
#![function!("../../crates/ch20_runtime/rt/src/apply.rs", closure)]
```

`rt_apply` roots its argument buffer, combines saved PAP arguments with new
ones, and repeats the eval/apply protocol from Chapter 19 [@marlow2004]. A short
call allocates a PAP, an exact call enters code, and an oversaturated call
continues with the returned closure and remaining arguments. `rt_apply1` through
`rt_apply4` use the same implementation. `rt_closure_set` checks and patches
capture slots only during unpublished recursive-group initialization.

The loop in apply does not guarantee native tail-call elimination through C
slow entries: the entry itself may call apply again. Chapter 21's known fast
calls need their own LLVM tail-call convention. The iterative ANF and Flat
machines already avoid recursive host calls, but that is a property of those
interpreters, not an automatic property of the C ABI.

`primitives.rs` separates a safe, typed value API from the word ABI in `abi.rs`.
All three interpreters adapt their data values to that safe API. They retain
their own control-flow engines and heap models. Native-facing exports decode
words according to their checked source types, call the shared semantics, and
encode the result. Unsafe accesses are confined to the runtime and documented
with typed, live-object contracts. Unwinding across an `extern "C"` entry aborts.
Language failures instead follow the explicit runtime error and exit paths.

`show_double` starts with Rust's shortest round-trip digits, then normalizes
notation. Nonzero magnitudes from `1e-4` inclusive to `1e16` exclusive use
positional notation. Other finite values use a signed exponent. Zero retains
its sign, and special values spell `NaN`, `Infinity`, and `-Infinity`. We check
boundary values through both the safe API and the word ABI, including whether
finite values survive a print-and-read round trip with the same bits. `readInt` accepts optional surrounding whitespace
and a decimal sign, then rejects values outside the signed 63-bit range.
Floating `round` uses ties-to-even and saturates. NaN converts to zero.

## Running It

The larger `double-specials` fixture also checks the positional/scientific
notation boundary:

```text
$ cd crates/ch20_runtime
$ cargo run -q -- run tests/programs/runtime/double-specials.fun --lint
-0.0
NaN
Infinity
-Infinity
0.0001
1.0e+16
$ cargo run -q -- dump --pass=types tests/programs/runtime/double-specials.fun
main : IO ()
```

From the repository root, `cargo test -p fun-rt-20` exercises primitive semantics
and the standalone ABI harness. `python3 scripts/check_runtime_abi.py` builds
the static runtime, asks rustc for the host's native static libraries, links the
C smoke program with the system compiler, and checks that it prints `42`.
`just build` also builds the runtime explicitly before the workspace. Linux
requires `libgc-dev`. MacOS uses Homebrew's `bdw-gc`, with `GC_LIB_DIR` available
for another installation location.

A collector bug often hides until allocation happens at exactly the wrong
moment. The runtime tests force collection while recursive closures are being
built and while calls hold live arguments. They also exercise partial and
oversaturated calls, Unicode strings, and output before errors. These are the
places where a correct-looking object layout has to work as part of a running
program.

## Exercises

1. **Warm-up.** Compute the encoded words for `-1`, `0`, `True`, and unit. Explain
   why equal words need not represent equal source values.
2. **Intermediate.** Draw a two-field constructor and a one-argument PAP, including
   their headers and payload sizes.
3. **Intermediate.** Explain why a live pointer stored only in an ordinary Rust
   vector can disappear from Boehm's view.
4. **Advanced.** Add a forced collection between recursive closure allocation and
   patching in an ABI test, keeping every unfinished object rooted.
5. **Advanced.** Identify what extra metadata a moving collector would need to
   update closure captures, PAP arguments, globals, and live local values.

## Further Reading

- Conservative collection without compiler cooperation [@boehm1988].
- Root discovery with compiler cooperation [@henderson2002].
- Representation choices in a strict functional runtime [@leroy1990].
- Eval/apply and partial applications [@marlow2004].
- Collection algorithms and implementation tradeoffs [@gc2023].
- Takenobu Tani's GHC (STG, Cmm, asm) Illustrated
  ([pdf](https://takenobu-hs.github.io/downloads/haskell_ghc_illustrated.pdf)
  / [github](https://github.com/takenobu-hs/haskell-ghc-illustrated)), for
  GHC's heap objects, stack layout, and calling convention next to ours.
