# Chapter 23: Optimization

> **New files**
>
> - `src/rep.rs`
> - `src/reach.rs`
> - `src/dump.rs`
>
> **Changed files**
>
> - `src/erase.rs`
> - `src/llvm/values.rs`
> - `src/llvm/primitives.rs`
> - `src/llvm/control.rs`
> - `src/llvm/mod.rs`
> - `src/anf.rs`
> - `src/anf_simplify.rs`
> - `src/anf_lint.rs`
> - `src/closure.rs`
> - `src/flat.rs`
> - `src/flat_lint.rs`
> - `src/primitives.rs`
> - `rt/src/primitives.rs`
> - `rt/src/abi.rs`
> - `src/main.rs`
> - `src/repl.rs`
> - `src/jit.rs`
> - `src/lib.rs`
> - `rt/src/lib.rs`

## Motivation

Getting the right answer is a good start for an optimizer. We'd also like it
to avoid doing the same calculation twice or allocating a fresh box between
every pair of arithmetic instructions. A faster-looking expression can turn
out to be a rather expensive way to get the same result.

We'll make a few focused improvements and inspect what they actually change.
Traces and machine steps help us catch duplicated or reordered work. A dependency
graph tells us which globals we still need. Type information tells codegen when
it can use native arithmetic instead of repeatedly boxing intermediate values.

These are small optimizations, but they expose the questions a larger optimizer
must answer too. What do we know about this expression? What does the rewrite
save? And what happens to evaluation order?

## The Language, Now

The only new source function is `trace :: String -> a -> a`. It writes its first
argument and a newline to stderr, then returns its second argument unchanged.
Arguments remain strict. An error while evaluating the second argument happens
before the trace is printed. `trace` is a diagnostic facility, not an alternative
to IO for ordinary program effects.

```fun
{{#include ../../crates/ch23_optimize/tests/programs/optimize/trace-arguments.fun}}
```

This prints `left` followed by `right` on stderr, and `42` on stdout. Moving a
call across the other argument would now be visible. The runtime flushes stdout
before writing a trace, preserving the ordering of preceding output.

## Theory

### Preserve Work

In a strict language, `let x = f y in x + x` evaluates `f y` once. Substituting
`f y` for both occurrences of `x` would evaluate it twice. That changes cost and
may repeat a trace. A failing expression is observable too. An unused division
by zero cannot simply disappear.

Our simplifier can substitute atoms because reading an atom does no work. It can
remove an unused total computation. It can move an immediately applied lambda
body into its sole call site, preserving the already evaluated arguments. It
cannot use that rule for a lambda called twice. These restrictions make the
small transformation understandable without a global cost analysis [@appel1992].

We can ask the ANF machine to count its steps with simplification switched
on and off. The results, errors, and traces must agree, and the simplified
program must take no more steps. This gives us a useful way to catch a rewrite
that accidentally duplicates work. It doesn't prove a wall-clock speedup, but
it tests a claim we can actually measure without depending on a particular CPU.

### Keep Reachable Globals

A global reference gives us an edge in a dependency graph. Starting from `main`,
a worklist follows those edges and retains a predecessor for each newly reached
global. This resembles Chapter 9's dependency analysis, but answers a different
question. Which definitions can this execution need?

Strict initialization adds roots. An unused initializer may print a trace, fail,
or diverge before `main`. We retain any initializer whose harmlessness the local
classifier cannot establish. Calls and control-flow initializers remain roots.
Allocating a closure does not execute its body, so an unused function can be
removed. Allocation failure is outside the source cost model.

The surviving globals stay in their original dependency order. In the REPL, all
visible bindings are roots because later input may refer to any of them. The
witness dump gives one discovery edge for each retained binding, making pruning
decisions inspectable.

### Keep Type Evidence, Choose Storage Locally

An erased expression no longer carries a System F type, but the compiler can
retain a small fact established by that type:

```rust
#![enum!("../../crates/ch23_optimize/src/rep.rs", Rep)]
```

`Int` and `Double` establish numeric types. `Word` covers everything else,
including type variables. This is evidence, not a storage layout. An `Int`
returned by a call still arrives as a tagged word. Code generation decides when
to decode it into a register. Calls, returns, fields, captures, globals, and join
parameters retain the uniform ABI from Chapter 20 [@leroy1990].

Integer register operations need one extra step. An LLVM `i64` has 64 bits,
while Fun's signed integer has 63. Adding $2^{61}$ to itself must produce
$-2^{62}$. Without normalization, a following comparison would see a positive
number, and division by two would produce the wrong sign. After arithmetic we
shift left once, then arithmetically right once. This sign-extends bit 62 and
restores Fun's range before any later use. Adding $2^{62}$ to itself instead
wraps to zero, not a negative number.

Double arithmetic can keep intermediate results in floating-point registers.
Passing the result to another function still requires a box because our calling
convention passes words. So a primitive Double accumulation loop still boxes
its accumulator once per iteration at the tail call. The improvement is that
it doesn't box every arithmetic intermediate along the way.

We'll check that distinction by counting allocations. Overloaded source
operators can still make dictionary calls, so they may not produce this direct
primitive loop. Specializing those dictionaries is a separate opportunity.

## Implementation

The shared runtime owns trace behavior. Both interpreters and the native ABI
use it, so even an arbitrary closure can be the returned value:

```rust
#![function!("../../crates/ch23_optimize/rt/src/primitives.rs", trace)]
```

The simplifier states what its rewrites are allowed to do beside the code.

```rust
{{#include ../../crates/ch23_optimize/src/anf_simplify.rs:rewrite-contract}}
```

`reach::analyze` performs a stable graph traversal. Its roots include observable
initializers, and the first edge to a global becomes its witness:

```rust
#![function!("../../crates/ch23_optimize/src/reach.rs", analyze)]
```

Erasure classifies types while they are still available:

```rust
#![function!("../../crates/ch23_optimize/src/erase.rs", representation)]
```

An explicit postorder worklist records binder and expression-result evidence.
ANF copies it to fresh temporaries, including the result of each application in
an oversaturated call. Closure conversion preserves it. Missing or contradictory
numeric evidence is an internal lint error. The interpreters ignore this table.
IR dumps show numeric annotations so the reader can inspect the facts themselves.

Within LLVM emission, a separate enum describes current storage:

```rust
#![enum!("../../crates/ch23_optimize/src/llvm/values.rs", Loc)]
```

`atom_word` handles ABI uses. Integer and Double primitive operands request native
storage instead. A tagged input is decoded once for that use, while a result
already in a native register can flow directly into the next primitive. Integer
arithmetic calls the normalization helper before storing its native result:

```rust
{{#include ../../crates/ch23_optimize/src/llvm/primitives.rs:normalize-int}}
```

To isolate this codegen change, we construct Flat loops that use primitives
directly and count runtime calls in the optimized LLVM. The integer loop needs
none. The Double loop needs one atomic allocation for the value crossing its
call boundary. We separately compare overloaded source programs with the
interpreter. That keeps us honest about which improvement we've implemented.

## Running It

From the repository root:

```text
$ cd crates/ch23_optimize
$ cargo run -q -- run tests/programs/optimize/trace-sharing.fun --engine=anf --print-steps
once
42
steps: 92
$ cargo run -q -- run tests/programs/optimize/trace-sharing.fun --engine=anf --print-steps --no-simplify
once
42
steps: 171
$ cargo run -q -- dump --pass=reach tests/programs/optimize/trace-sharing.fun
```

The last command lists the retained definitions and their witnesses, ending with
the number kept and dropped. Use `dump --pass=anf` or `dump --pass=flat` to inspect
numeric binder annotations. To examine the actual optimized source loop:

```text
$ cargo run -q -- dump --pass=llvm-opt tests/programs/optimize/double-loop.fun
```

Follow the dictionary calls as well as the local floating-point instructions.
Comparing only the number of instructions in the source function can hide work
inside its callees. `cargo test --test optimization` runs the work oracle and
trace corpus. `cargo test --test native` includes the allocation-count contract.

## Exercises

1. **Warm-up.** Predict the trace order when the second argument to `trace` is
   another traced expression. Confirm it on all engines.
2. **Intermediate.** Extend the reachability dump to show the complete path from
   a root rather than one predecessor.
3. **Intermediate.** Add `Bool` evidence and keep comparison results untagged until
   they cross a word boundary.
4. **Advanced.** Carry native integers through join parameters. State the new
   storage invariant and extend lint before changing code generation.
5. **Advanced.** Extend the work oracle with a counter for dead stores. Construct
   a rewrite that preserves transition counts but performs an unnecessary store.

## Further Reading

- Transformation and cost reasoning in a compiler [@appel1992].
- Value representation in a strict functional implementation [@leroy1990].
