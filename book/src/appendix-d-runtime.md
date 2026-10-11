# Appendix D: Runtime ABI

This appendix describes the final native compiler and `fun-rt-24`. Supported
hosts use 64-bit words and pointers: Apple Silicon macOS and x86-64/AArch64 Linux.
The ABI is internal to this compiler edition. There is no user-written FFI.

## Words and Objects

Every source value crossing a call boundary is an LLVM `i64` and a Rust `u64`.
A low bit of one denotes an immediate. A low bit of zero denotes an aligned
object address, except for temporary zero placeholders during initialization.

| Value               | Encoding                                         |
| :------------------ | :----------------------------------------------- |
| `Int`               | `(n << 1) \| 1`, signed 63-bit two's complement  |
| `Char`              | Unicode scalar shifted left one bit, low bit set |
| Nullary constructor | `(tag << 1) \| 1`                                |
| Heap value          | Address of its header, aligned to eight bytes    |

The expected type determines the interpretation. Thus `False`, `[]`, `Nothing`,
and `()` may all have word value `1` without being interchangeable.

```text
header:  [ payload size: 32 bits | constructor tag: 24 bits | kind: 8 bits ]
object:  [ header ][ payload word 0 ][ payload word 1 ] ...
```

Size counts payload words. Constructor tags are local to their data type and
follow declaration order. Fixed Prelude tags are `False=0`, `True=1`, `[]=0`,
`(:)=1`, `Nothing=0`, `Just=1`, `()=0`, and `LT=0`, `EQ=1`, `GT=2`.

| Kind      | ID | Payload                                      | Traced fields         |
| :-------- | -: | :------------------------------------------- | :-------------------- |
| `CON`     |  0 | Constructor fields                           | All                   |
| `CLOSURE` |  1 | Slow code address, raw arity, captures       | Captures              |
| `PAP`     |  2 | Closure word, raw argument count, arguments  | Closure and arguments |
| `DOUBLE`  |  3 | Binary64 bits                                | None                  |
| `STRING`  |  4 | Raw byte length, UTF-8 bytes padded to words | None                  |
| `FORWARD` |  5 | Destination address during collection        | Collector-only marker |

There is no heap allocation for a nullary constructor. Every movable object
therefore has room for a forwarding address. Strings count bytes in storage and
Unicode scalars in `length`-style primitives.

## Entries and Application

| Entry                   | Convention and signature                                      |
| :---------------------- | :------------------------------------------------------------ |
| Known function          | `tailcc i64 f(i64 env, i64 a1, ..., i64 an)`                  |
| Slow adapter            | C: `u64 slow(u64 closure, const u64 *args)`                   |
| General apply           | C: `u64 rt_apply(u64 closure, usize n, const u64 *args)`      |
| Small apply             | C: `u64 rt_applyN(u64 closure, u64 a1, ..., u64 aN)`, N=1..4  |
| PAP construction        | C: `u64 rt_make_pap(u64 closure, usize n, const u64 *args)`   |
| Recursive capture patch | C: `void rt_closure_set(u64 closure, usize index, u64 value)` |

The slow adapter receives exactly the function's syntactic arity. The general
apply routine flattens PAP arguments, invokes exact calls, or applies remaining
arguments to a returned function. Only known tail calls have a guaranteed native
tail transfer. Crossings between C and `tailcc` retain their ABI adapter frame.

All arguments are strict and already evaluated. Raw argument arrays must remain
readable for the call. A collector may relocate their _values_, so persistent
buffers belong to a root owner. A slow entry loads its arguments and establishes
its generated root frame before any allocation.

## Allocation and Roots

| Export                                    | Contract                                                     |
| :---------------------------------------- | :----------------------------------------------------------- |
| `void rt_init()`                          | Initialize Boehm and register the calling runtime thread     |
| `u64 *rt_alloc(usize words)`              | Reserve scanned storage, including header                    |
| `u8 *rt_alloc_atomic(usize bytes)`        | Reserve pointer-free storage                                 |
| `void rt_use_copying()`                   | Select the single-threaded moving heap before initialization |
| `void rt_roots_push(u64 *slots, usize n)` | Register a writable, initialized generated root range        |
| `void rt_roots_pop()`                     | Remove the current generated root range                      |
| `void rt_global_root(u64 *slot)`          | Register a static global for the process lifetime            |

Allocation can collect before returning. The caller initializes the entire new
object before any further allocation. Native startup registers globals before
installing functions or evaluating values. Static string objects are immutable
and stationary. The Boehm backend also retains their address table.

Rust's `Roots` and `GlobalRoot` owners register stable buffers until drop.
The moving collector updates their cells. Borrowed string slices and unrooted
copies of pointer words must not survive an allocation. Primitive exports decode
strings into owned Rust values before allocating their result.

## Primitives, Errors, and Exit

Every `prim_*` operation has a C export named `rt_prim_*`. Each argument and the
return value is a word, including a unit result. Integer and Double arithmetic
may instead be emitted inline. Nontrivial operations share their safe Rust
implementation with the interpreters.

| Primitive family                      | Arguments                       | Result                   |
| :------------------------------------ | :------------------------------ | :----------------------- |
| `intAdd/Sub/Mul/Quot/Rem/Div/Mod`     | Two Int words                   | Int word                 |
| `intNeg`                              | Int                             | Int                      |
| `intEq/Lt/Le`                         | Two Int words                   | Bool immediate           |
| `dblAdd/Sub/Mul/Div/Pow`              | Two boxed Doubles               | Boxed Double             |
| `dblNeg/Sqrt/Exp/Log/Sin/Cos`         | Boxed Double                    | Boxed Double             |
| `dblEq/Lt/Le`                         | Two boxed Doubles               | Bool immediate           |
| `intToDbl`, `dblFloor/Round/Truncate` | Source numeric value            | Converted numeric value  |
| `chrOrd`, `chrChr`, `chrEq/Lt`        | Character/integer operands      | Integer/character/Bool   |
| `strAppend/Length/Eq/Compare`         | One or two Strings              | String/Int/Bool/Int      |
| `strUnpack`, `strPack`                | String or character list        | Character list or String |
| `showInt/Double/Char/String`          | One value of the named type     | String                   |
| `readInt`                             | String                          | `Maybe Int`              |
| `putStr`, `getLine`                   | String or unit                  | Unit or String           |
| `trace`                               | String message, arbitrary value | The same value           |
| `error`, `exit`                       | String or Int                   | Does not return          |

Integer results wrap modulo 2⁶³. Floating conversion saturates to the Int range,
with NaN becoming zero. Zero division, invalid characters, EOF on `getLine`, and
failed matches are runtime errors.

| Export                                                 | Behavior                                   |
| :----------------------------------------------------- | :----------------------------------------- |
| `rt_error(u64 string)`                                 | Flush stdout, write `fun: MESSAGE`, exit 1 |
| `rt_pattern_fail(const char *file, u32 line, u32 col)` | Report source location, exit 1             |
| `rt_div_zero()`                                        | Report `fun: divide by zero`, exit 1       |
| `rt_exit(i32 code)`                                    | Flush stdout and terminate                 |

Generated C `main` initializes the runtime, evaluates globals in dependency
order, applies source `main` to unit word `1`, and exits successfully. A runtime
panic aborts rather than unwinding through generated code.
