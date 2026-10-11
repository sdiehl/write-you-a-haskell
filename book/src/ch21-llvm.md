# Chapter 21: LLVM Code Generation

> **New files**
>
> - `src/llvm/mod.rs`
> - `src/llvm/control.rs`
> - `src/llvm/values.rs`
> - `src/llvm/primitives.rs`
> - `src/native.rs`
>
> **Changed files**
>
> - `src/flat.rs`
> - `build.rs`
> - `src/flat_lint.rs`
> - `src/lib.rs`
> - `src/main.rs`

## Motivation

A compiler's back end is a collection of agreements. A field load must agree
with the allocator about object layout. A call must agree with its callee about
arguments and registers. A branch must preserve the evaluation order established
before it. LLVM supplies instruction selection, register allocation, and machine
optimization. These agreements remain our responsibility.

The preceding chapters have made this boundary small. Flat IR already names
every intermediate value, lifts every function, distinguishes known calls from
closure applications, and makes tail transfers explicit. This chapter translates
those decisions into LLVM without revisiting source-language inference or
pattern matching. The result is the first standalone native Fun compiler.

## The language, now

The language is unchanged. `fun-ch21 build FILE -o OUT` produces an executable.
`run` still selects an interpreter. `-O0` through `-O3` control optimization,
with `-O2` the default. `--dump=llvm` shows the module before optimization and
`--dump=llvm-opt` shows it afterward.

The acceptance program brings together records, dictionaries, higher-order
functions, IO, and a strict accumulator loop:

```fun
{{#include ../../crates/ch21_llvm/tests/programs/native/acceptance.fun}}
```

The final call performs ten million iterations. Its correctness includes space
behavior: the native call stack must remain bounded.

## Theory

LLVM's static single assignment form gives each instruction result one
definition. Our unique Flat binders already provide that discipline. A `Let`
associates its name with an LLVM value. A `Join` creates a basic block, and each
join parameter becomes a phi node. A `Jump` supplies incoming values and branches
to that block. Join points express this shared continuation directly
[@maurer2017].

A phi does not select a value by testing a condition. It selects the value paired
with the predecessor block through which control arrived. This matters when
emitting one Flat operation creates several LLVM blocks: division inserts a
zero check, and a mixed constructor case distinguishes an immediate from a
pointer. A jump records the builder's **current** block after emitting its
arguments, not the block in which emission originally began.

| Flat construct  | LLVM translation                                  |
| --------------- | ------------------------------------------------- |
| `Let`           | Instructions and a name-to-value entry            |
| `Case`          | Tag inspection and `switch`                       |
| `Join`          | A block with phi parameters                       |
| `Jump`          | Phi incoming values and `br`                      |
| Known tail call | `tail call tailcc`, immediately followed by `ret` |
| Unknown call    | C-convention runtime apply                        |

Tail position alone is insufficient for bounded native stack use. LLVM requires
compatible calling conventions and a suitable return sequence. We give every
fast entry `tailcc`, mark known tail calls, and immediately return the call's
result. Internal linkage also meets the visibility requirement for x86-64
position-independent code. These choices follow LLVM's
[tail-call rules](https://llvm.org/docs/CodeGenerator.html#tail-call-optimization).
The guarantee applies to known saturated calls, including mutual recursion.
It does not extend across the runtime's C eval/apply boundary.

## Implementation

### Establish the target first

`llvm/mod.rs` owns the module, builder, target machine, and symbol tables. Its
LLVM values borrow a `Context` owned by the caller. This makes the lifetime
boundary explicit and avoids a self-referential Rust structure. All function
entries and global slots are declared before bodies, so forward and mutually
recursive calls need no special case.

The chapter pins Inkwell 0.10.0 with `llvm22-1-prefer-dynamic` and LLVM 22. Target initialization runs once. The target machine uses the host
triple, the generic CPU, and position-independent relocation. Its triple and
data layout are installed on the module **before** emission. A 32-bit pointer
target is rejected. The smaller examples in
[Compiler Crates](https://sdiehl.github.io/compiler-crates/inkwell.html)
as a companion introduction.

Builder operations return errors. Flat lint checks the input, and LLVM's verifier
checks every completed module, regardless of `--lint`. Optimization uses the
new pass manager and verifies again:

```rust
#![function!("../../crates/ch21_llvm/src/llvm/mod.rs", Backend::optimize)]
```

Verification detects malformed LLVM, such as a missing terminator, a wrong call
type, or an invalid phi. It cannot establish Fun's semantics. A well-formed
module can still evaluate arguments in the wrong order or overflow incorrectly.
Those are differential-test obligations.

### Traverse with a worklist

The emitter never recursively descends the Flat tree. Pending blocks live in a
vector, and a loop consumes each straight-line `Let` spine. The terminal handler
emits a return or transfer, or queues the bodies of joins and alternatives.

The environment is a table of unique SSA definitions. Lexical scope was checked
by Flat lint, so emission does not clone a scope map at each branch. A join's
phis are created before its callers are visited. Incoming edges can be added
as those callers are emitted. Each completed block has exactly one terminator.

This chapter also makes Flat lint and printing iterative. Lint uses explicit
enter/leave events to maintain lexical scopes. Owned Flat program destruction
drains `Rc` edges through a worklist: otherwise Rust's generated destructors
could recursively free a deep tree after compilation had succeeded. A test
constructs 50,000 bindings and 5,000 nested joins, then checks, prints, emits,
verifies, and drops them on a 512 KiB thread stack. This establishes the bound
for our Flat traversal code. Inherited front-end passes and the Core reference
interpreter still use recursion.

```rust
#![function!("../../crates/ch21_llvm/src/llvm/control.rs", Backend::body)]
```

### Preserve the word representation

Every Fun value is an LLVM `i64`, using Chapter 20's shared `runtime_layout`.
Loads convert a word to a pointer and index an array of words. Inkwell's opaque
pointers require the accessed element type explicitly:

```rust
#![function!("../../crates/ch21_llvm/src/llvm/values.rs", Backend::slot)]
```

The `unsafe` boundary constructs LLVM. It does not dereference a Rust pointer.
The generated access still depends on checked constructor arity and the runtime
layout. We use ordinary GEP rather than claiming additional `inbounds`
guarantees. A constructor field is one word past the header. A closure capture
also skips the slow-entry address and arity.

Nullary constructors are immediates. Other constructors are allocated with
`rt_alloc`, then initialized in slot order. A case on a type containing both
forms tests the low bit before loading a header. Types containing only one
form need no such branch. Integer and character literal cases switch directly
on tagged words. String equality has already become primitive tests during
match compilation.

String literals are aligned, read-only objects containing their header, byte
length, and padded UTF-8 bytes. A retained data table records their root words.
`llvm.used` keeps that table through optimization and linking. These static
objects are never moved or freed. Heap strings and doubles use atomic allocation
because they contain no pointers. Generated globals and live machine
registers/stack slots remain visible to Boehm [@boehm1988].

Integer operations decode with arithmetic shift, compute in `i64`, and retag.
No `nsw` or `nuw` flags are attached: wrapping is defined behavior. A zero divisor
branches to `rt_div_zero` before division. The smallest Fun integer divided by
minus one fits in the wider intermediate `i64`. Retagging supplies the required
63-bit wrap. `div` and `mod` adjust truncating quotient and remainder when their
signs require flooring.

Double operations unbox, use IEEE instructions without fast-math flags, and
rebox. Ordered comparisons preserve NaN behavior. Transcendentals, saturating
conversions, text, formatting, and IO call the same runtime used by the
interpreters. LLVM optimization must not replace those language-level choices.

### Separate fast calls from closure application

Each lifted function has two entries. The fast entry receives an environment
word followed by ordinary word arguments, with `tailcc`. A top-level function
receives zero for its unused environment. Known calls set the convention on
the **call instruction** as well as on the callee:

```rust
#![function!("../../crates/ch21_llvm/src/llvm/values.rs", Backend::known)]
```

The slow entry uses the C ABI, taking the closure word and an argument-array
pointer. It unpacks exactly the function's syntactic arity and enters fast code.
Closures store this slow address. Unknown calls use `rt_apply1` through
`rt_apply4`, or the general `rt_apply` for wider batches. General argument arrays
are allocated in the function's entry block, not repeatedly at a control-flow
site. The runtime handles partial and oversaturated applications using the
existing eval/apply protocol [@marlow2004].

The C adapter is not a guaranteed tail transfer, nor is an unknown tail call
through `rt_apply`. Higher-order recursion can therefore grow the native stack.
Extending the guarantee requires an LLVM-emitted apply path with a compatible
calling convention. Keeping this boundary explicit is more useful than treating
an optional optimizer transformation as a language guarantee.

### Initialize, emit, and link

`fun_init` installs global function closures first, then evaluates strict value
initializers in dependency order. The generated C `main` initializes the runtime,
calls `fun_init`, applies the source `main` to unit, and exits through `rt_exit`
to flush output. This order preserves which top-level error occurs first and
ensures merely constructing an IO action performs no effects.

`native.rs` writes an object with the target machine, then invokes the system
`cc` directly. It links `libfun_rt_21.a`, Boehm, and the native library flags
reported by rustc. `FUN_RT` selects an installed archive. Otherwise it is found
beside the compiler. `just build` prepares the runtime and flags first. Linking
uses a temporary directory beside the destination and installs the executable
only on success. There is no shell interpolation and a failed link leaves an
existing output intact.

Calls crossing between `tailcc` and the C runtime are explicitly `notail`.
Without that restriction, LLVM can infer an ABI-crossing tail transfer whose
stack cleanup is wrong for wide calls. Chapter 22's JIT exposed this AArch64
failure. The correction is applied here too. Known Fun-to-Fun tail calls retain
their guarantee.

## Running it

Install LLVM 22 and Boehm as described in the repository README. Set
`LLVM_SYS_221_PREFIX` to the LLVM installation prefix, then build from the root:

```text
$ just build
$ target/debug/fun-ch21 build crates/ch21_llvm/tests/programs/native/acceptance.fun -o build/acceptance --lint
$ build/acceptance
5.0
3.0
6.0
[3.0,10.0]
True
50000005000000
$ target/debug/fun-ch21 dump --pass=llvm crates/ch21_llvm/tests/programs/native/acceptance.fun > build/acceptance.ll
$ target/debug/fun-ch21 dump --pass=llvm-opt crates/ch21_llvm/tests/programs/native/acceptance.fun > build/acceptance-opt.ll
```

The corpus now contains 417 programs. Native output, the first stderr line,
and exit status are compared with the interpreters wherever each fixture's
engine header permits. The large acceptance and mutual-recursion tests use ten
million calls. A smaller acceptance program runs on all four engines. The
1,000-case generator also includes native execution. Ten LLVM snapshots expose
instruction-level changes, while `-O0` through `-O3` tests cover representation,
argument order, wide calls, and global initialization.

Separate stress tests execute the native recursion programs with a 512 KiB
stack. Another loop allocates at least 100 million words under a 64 MiB Boehm
heap limit. Together these test two different obligations: tail transfers must
release call frames, and dead heap objects must remain collectible. Passing
LLVM verification alone establishes neither.

## Exercises

1. **Warm-up:** trace a field load from a tagged word through `inttoptr`, GEP,
   and `load`. State which earlier check justifies the field index.
2. **Intermediate:** draw the blocks for a join with two callers. Add division
   in one caller and identify the correct phi predecessor afterward.
3. **Intermediate:** explain why adding `nsw` to integer multiplication would
   change Fun's semantics, even if most examples still pass.
4. **Advanced:** compare unoptimized and optimized IR for `sumTo`. Check the
   calling convention at both ends of its recursive transfer.
5. **Advanced:** design an unknown-call stress test that exposes the C apply
   boundary. Describe the ABI change needed to make it a guaranteed tail call.

## Further reading

- [Compiler Crates: Inkwell](https://sdiehl.github.io/compiler-crates/inkwell.html),
  for building LLVM modules through Rust.
- [LLVM Language Reference](https://llvm.org/docs/LangRef.html), especially
  calls, phi nodes, integer overflow flags, and pointer arithmetic.
- [LLVM tail-call implementation](https://llvm.org/docs/CodeGenerator.html#tail-call-optimization),
  for the conditions behind a bounded-stack claim.
- Join points and shared continuations [@maurer2017].
- Fast application and the eval/apply boundary [@marlow2004].
- Representation in a strict functional implementation [@leroy1990].
