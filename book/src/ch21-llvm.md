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

We can now run Fun programs, but they still need our interpreter to do the
work. In this chapter we'll produce an executable you can launch from the shell.
That's a pretty satisfying destination for a project that began with a calculator!

Much of the preparation is already behind us. Pattern matching has become
branches, closures have explicit environments, and Flat IR tells us where calls
and tail transfers belong. LLVM will handle instruction selection, register
allocation, and machine optimization.

We still have to tell it how our values fit in memory and how our functions
call one another. If those decisions disagree with the runtime, LLVM can
efficiently compile the wrong program. We'll work through that boundary carefully,
using the representations we already understand to keep the new work manageable.

## The Language, Now

The language is unchanged. `fun-ch21 build FILE -o OUT` produces an executable.
`run` still selects an interpreter. `-O0` through `-O3` control optimization,
with `-O2` the default. `--dump=llvm` shows the module before optimization and
`--dump=llvm-opt` shows it afterward.

The acceptance program brings together records, dictionaries, higher-order
functions, IO, and a strict accumulator loop:

```fun
{{#include ../../crates/ch21_llvm/tests/programs/native/acceptance.fun}}
```

The final call performs ten million iterations. Getting the number right is
only part of the challenge. The native stack should stay bounded too, just as
it does for a tail call in our explicit machines.

## Theory

LLVM uses _static single assignment_, or SSA. Each instruction result has one
definition, so a name always identifies the same computed value. Our unique
Flat binders already fit this idea. A `Let` associates a binder with an LLVM
value, while a `Join` becomes a basic block that several paths can enter.

The interesting question is how those paths pass values to the block. LLVM
uses _phi nodes_. Each join parameter becomes a phi, and a `Jump` supplies its
incoming value before branching to the block. The join points we've already
built give this translation a natural shape [@maurer2017].

Imagine two paths that meet at a join. One supplies `10`, the other `20`.
The phi takes the value associated with whichever path we arrived from. It
doesn't test the original condition again.

We can write that merge as a tiny LLVM fragment. Assume the condition is
already an `i1`. We use raw integers here only to isolate the control flow.

```llvm
left:
  br label %merge
right:
  br label %merge
merge:
  %value = phi i64 [ 10, %left ], [ 20, %right ]
  ret i64 %value
```

An earlier conditional branch chooses `left` or `right`. When we enter
`merge`, the predecessor selects the incoming value. Our real translation
supplies tagged Fun words, but the correspondence is the same. A join
parameter becomes `%value`, and each jump contributes one incoming pair.

There is an easy codegen mistake here. Emitting one Flat operation can create
several LLVM blocks. Division, for example, adds a zero check. The phi's incoming
edge must name the block we actually leave after that check, not the block
where we started emitting the expression. Record the builder's **current**
block after emitting the jump arguments.

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
target is rejected. For a smaller introduction to the bindings, see the examples
in [Compiler Crates](https://sdiehl.github.io/compiler-crates/inkwell.html).

Builder operations return errors. Flat lint checks the input, and LLVM's verifier
checks every completed module, regardless of `--lint`. Optimization uses the
new pass manager and verifies again:

```rust
#![function!("../../crates/ch21_llvm/src/llvm/mod.rs", Backend::optimize)]
```

The verifier can catch a missing terminator, an invalid phi, or a call with
the wrong type. It's an excellent first line of defence. But perfectly valid
LLVM can still be a wrong translation of Fun, perhaps evaluating arguments
in the wrong order or mishandling integer overflow. We catch those mistakes by
running the same programs through the interpreters and native code.

### Traverse with a worklist

The emitter never recursively descends the Flat tree. Pending blocks live in a
vector, and a loop consumes each straight-line `Let` spine. The terminal handler
emits a return or transfer, or queues the bodies of joins and alternatives.

The environment is a table of unique SSA definitions. Lexical scope was checked
by Flat lint, so emission does not clone a scope map at each branch. A join's
phis are created before its callers are visited. Incoming edges can be added
as those callers are emitted. Each completed block has exactly one terminator.

The worklist approach also applies to lint, printing, and cleanup. That last
one is easy to forget. Rust's generated destructors can recursively free a deep
`Rc` tree after compilation has apparently succeeded. We drain those edges with
a worklist too. Deep Flat trees are checked, printed, emitted, verified, and
released on a small Rust stack. This checks our Flat traversals, rather than
assuming a successful code-generation loop makes every other pass safe.

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
We attach neither `nsw` nor `nuw`, since wrapping is defined behavior. A zero divisor
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
Otherwise LLVM can infer a tail transfer across the two conventions and get
stack cleanup wrong for a wide call. This is a particularly unpleasant bug
because the IR can pass verification. Marking this boundary doesn't interfere
with our guaranteed tail calls between known Fun functions.

## Running It

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

Run the examples through both native code and the interpreters. Compare errors
as well as successful answers, since evaluation-order bugs often reveal
themselves in which error appears first. It's also worth trying all optimization
levels. A calling-convention mistake can hide until LLVM decides to make a
particular transformation.

Separate stress tests execute the native recursion programs with a 512 KiB
stack. Another loop allocates at least 100 million words under a 64 MiB Boehm
heap limit. Together these test two different obligations. Tail transfers must
release call frames, and dead heap objects must remain collectible. Passing
LLVM verification alone establishes neither.

## Exercises

1. **Warm-up.** Trace a field load from a tagged word through `inttoptr`, GEP,
   and `load`. State which earlier check justifies the field index.
2. **Intermediate.** Draw the blocks for a join with two callers. Add division
   in one caller and identify the correct phi predecessor afterward.
3. **Intermediate.** Explain why adding `nsw` to integer multiplication would
   change Fun's semantics, even if most examples still pass.
4. **Advanced.** Compare unoptimized and optimized IR for `sumTo`. Check the
   calling convention at both ends of its recursive transfer.
5. **Advanced.** Design an unknown-call stress test that exposes the C apply
   boundary. Describe the ABI change needed to make it a guaranteed tail call.

## Further Reading

- [Inkwell in Compiler Crates](https://sdiehl.github.io/compiler-crates/inkwell.html),
  for building LLVM modules through Rust.
- [LLVM Language Reference](https://llvm.org/docs/LangRef.html), especially
  calls, phi nodes, integer overflow flags, and pointer arithmetic.
- [LLVM tail-call implementation](https://llvm.org/docs/CodeGenerator.html#tail-call-optimization),
  for the conditions behind a bounded-stack claim.
- Join points and shared continuations [@maurer2017].
- Fast application and the eval/apply boundary [@marlow2004].
- Representation in a strict functional implementation [@leroy1990].
