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

Our execution engines agree on answers. That gives us a useful starting point
for optimization, but it does not tell us whether a transformation evaluates a
function twice, retains an unused dictionary, or allocates a box between every
pair of arithmetic instructions. Agreement on results is only part of the
optimizer's obligation.

There are two mistakes to guard against. A rewrite can duplicate or reorder work.
It can also act on a fact the compiler never established, such as treating a
polymorphic word as an unboxed integer. This chapter makes both obligations
visible: traces and machine steps test work preservation, a graph explains which
globals survive, and type evidence survives erasure to guide native arithmetic.

## The language, now

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
may repeat a trace. A failing expression is observable too: an unused division
by zero cannot simply disappear.

Our simplifier can substitute atoms because reading an atom does no work. It can
remove an unused total computation. It can move an immediately applied lambda
body into its sole call site, preserving the already evaluated arguments. It
cannot use that rule for a lambda called twice. These restrictions make the
small transformation understandable without a global cost analysis [@appel1992].

The work oracle runs each successful ANF fixture with and without simplification.
It compares stdout, complete stderr, exit status, and machine transitions. The
simplified execution must take no more transitions. This is a regression check
for our chosen machine, not a theorem about wall-clock time or every possible
input. Trace fixtures additionally run through all five engines.

### Keep Reachable Globals

A global reference gives us an edge in a dependency graph. Starting from `main`,
a worklist follows those edges and retains a predecessor for each newly reached
global. This resembles Chapter 9's dependency analysis, but answers a different
question: which definitions can this execution need?

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
Crossing a word boundary still requires a box. A primitive Double accumulation
loop therefore allocates one accumulator box per iteration at its tail call.
Our contract counts that box and rejects intermediate arithmetic boxes. Source
operators may still invoke dictionaries, so this chapter does not promise that
all overloaded source loops have that primitive shape. Dictionary specialization
would be a separate optimization.

## Implementation

The shared runtime owns trace behavior. Both interpreters and the native ABI
use it, so even an arbitrary closure can be the returned value:

```rust
#![function!("../../crates/ch23_optimize/rt/src/primitives.rs", trace)]
```

The simplifier records its local contract beside the code:

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

The codegen contract builds explicit primitive Flat loops, optimizes them with
LLVM, and counts runtime calls within each function. The integer loop has none.
The Double loop has exactly one 16-byte atomic allocation for its call-boundary
box. Separate source fixtures check overloaded loops and mixed values against
the reference interpreter. This separates a local code-generation guarantee
from optimizations the compiler has not implemented.

## Running it

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

1. **Warm-up:** predict the trace order when the second argument to `trace` is
   another traced expression. Confirm it on all engines.
2. **Intermediate:** extend the reachability dump to show the complete path from
   a root rather than one predecessor.
3. **Intermediate:** add `Bool` evidence and keep comparison results untagged until
   they cross a word boundary.
4. **Advanced:** carry native integers through join parameters. State the new
   storage invariant and extend lint before changing code generation.
5. **Advanced:** extend the work oracle with a counter for dead stores. Construct
   a rewrite that preserves transition counts but performs an unnecessary store.

## Further reading

- Transformation and cost reasoning in a compiler [@appel1992].
- Value representation in a strict functional implementation [@leroy1990].
