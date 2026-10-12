# Chapter 18: A-Normal Form and Join Points

> **New files**
>
> - `src/erase.rs`
> - `src/anf.rs`
> - `src/anf_simplify.rs`
> - `src/anf_machine.rs`
> - `src/anf_lint.rs`
>
> **Changed files**
>
> - `src/lib.rs`
> - `src/main.rs`

## Motivation

Typed Core makes type correctness explicit, but its nested expressions leave
many execution steps implicit. Before calling a function, an evaluator must
walk its argument expressions, preserve intermediate values, and remember what
to do after the call. A code generator needs that order written down.

A-normal form, or ANF, gives each intermediate computation a name and restricts
operation operands to atoms. We can then interpret the result with an explicit
continuation stack. The machine revisits the CEK design from Chapter 5, now with
constructors, dictionaries, IO, and join points. It is both a route toward native
code and an independent check on the reference interpreter.

## The language, now

The source language is unchanged. Consider a case expression used as an operand:

```fun
{{#include ../../crates/ch18_anf/tests/programs/anf/case-continuation.fun}}
```

Whichever alternative returns a value, the program must add three and print
once. ANF makes that common continuation explicit as a join rather than copying
it into each alternative. The new `--engine=anf` selects the iterative machine
and is the default. `--engine=core` retains the reference interpreter.

`--no-simplify` exposes the ANF produced before the optional rewrites. Both forms
must preserve stdout, the first stderr line, and process status. In particular,
strict evaluation still runs an unused binding's initializer. A failing division
cannot disappear merely because its result has no uses.

## Theory

An atom is a literal or a reference to an already available local or global
value. Reading a well-scoped atom performs no language computation. A
right-hand side performs one operation on atoms, and a `Let` names its result.
This separation exposes the sequencing implicit in nested call-by-value terms.
Flanagan and colleagues develop the connection between this style of direct
representation and continuation-based compilation [@flanagan1993].

Conversion receives a context describing what to do with a result. At the end
of a function it returns the atom. Inside an expression it binds the atom and
continues. At the end of a case alternative it can jump to a shared continuation.
This representation keeps joins local and second-class, following the motivation
for join points discussed by Maurer and colleagues [@maurer2017]. It does not
turn every function into a function that explicitly receives a continuation.

Type erasure is justified by the role of System F's type abstractions and
applications: they select and describe computations but do not themselves
perform runtime work [@pierce2002]. Dictionary arguments are different. They
contain actual method implementations and must remain as ordinary values.
IO already has its unit-function representation before erasure.

Tail position is a property of the remaining work. If a function call supplies
the current function's result directly, the machine needs no return frame for
that activation. A non-tail call must remember its pending computation. Constant
host-stack use therefore does not imply constant memory for every recursive
program: pending non-tail work can still grow the explicit continuation stack.
This is the practical advantage of making the CEK continuation visible
[@felleisen1986].

## Implementation

`erase::erase` preserves the order of global initialization while removing
executable type information. A postorder worklist drops `TyLam` and `TyApp`,
keeps dictionary terms, and rejects any nested `Match` that escaped the previous
pass. Read-only constructor and location metadata remains available for
primitives, printers, and diagnostics.

```rust
#![function!("../../crates/ch18_anf/src/erase.rs", erase)]
```

The ANF control language separates ordinary bindings, recursive closures,
branches, joins, and terminal transfers. `Fail` preserves the source span of a
pattern failure:

```rust
#![enum!("../../crates/ch18_anf/src/anf.rs", Anf)]
```

The converter's context has only three cases. `finish` interprets a context once
the result has become an atom. Pending conversions live in a work vector, so a
long expression does not become a long chain of Rust calls:

```rust
#![enum!("../../crates/ch18_anf/src/anf.rs", Context)]
#![function!("../../crates/ch18_anf/src/anf.rs", Convert::finish)]
```

For a non-tail case, conversion gives the remaining context a fresh join
parameter and converts each alternative with a jump context. Existing Core joins
survive. Adjacent lambdas merge into a multi-parameter lambda. This is syntactic
arity, not arity inferred from a function type.

Application conversion must respect the order of curried calls. In `f a b`,
the call `f a` may fail before `b` is evaluated. Combining both arguments into a
single eager batch would change which error appears first. The converter batches
only up to a known syntactic arity. Unknown calls consume one argument at a time,
and over-application preserves the boundary after the known prefix. The
`known-prefix-error` regression checks this ordering explicitly.

`anf_simplify` propagates atoms, selects known-constructor branches, removes
unused total right-hand sides, and reduces immediately applied saturated
lambdas. Calls, effects, and potentially failing primitives are retained. The
beta rule also checks whether the function escapes: passing a function to itself,
as in the inherited `id id` example, means its binding cannot simply be deleted.
Structural ANF lint independently checks names, scopes, join arities, and fresh
binders before execution.

The machine is a loop over code, application, and return states. Its frames
remember a non-tail binding continuation or arguments left after an
oversaturated call:

```rust
#![enum!("../../crates/ch18_anf/src/anf_machine.rs", State)]
#![enum!("../../crates/ch18_anf/src/anf_machine.rs", Frame)]
```

Closures carry a lambda and a shared lexical environment. Recursive groups are
built once in a machine-owned table. Their closures hold the group's index,
which avoids reference-count cycles and rebuilding closures at every call. Applying too few arguments
returns a partial application. Excess arguments are applied to the returned
function. Tail calls replace the current state, and jumps enter a lexically
captured join environment. Neither transfer recursively invokes the Rust
interpreter. Primitive values cross a data-only adapter to the existing
primitive implementation. Control flow does not call the Core evaluator.

## Running it

```text
$ cd crates/ch18_anf
$ cargo run -q -- run tests/programs/anf/case-continuation.fun --lint
7
$ cargo run -q -- dump --pass=types tests/programs/anf/case-continuation.fun
main : IO ()
```

Use `--pass=erase` to see type-free Core and `--pass=anf` to see named operations.
Repeat the latter with `--no-simplify` to distinguish conversion from rewriting.
Repeat execution with `--engine=core` to compare the reference result. Dumps
include the Prelude. The small snapshots in `tests/anf.rs` isolate individual
transformations such as a non-tail case and a guard-failure join.

The corpus now contains 394 programs. In addition, `testkit/src/gen.rs` creates
1,000 deterministic well-typed integer programs using arithmetic, local bindings,
branches, finite recursion, and lists. Twenty cases share each compilation to
amortize the Prelude. Their printed results must agree with both engines and an
independent wrapping-integer oracle. Failures retain the seed and source.

A separate one-million-call sum returns `500000500000` while recording fewer
than 32 pending continuations. That assertion tests bounded continuation depth,
not merely whether a machine happens to finish. The Core oracle also uses
explicit continuations, independently of ANF conversion. Non-tail calls may grow
either machine's heap-allocated continuation stack, but do not consume Rust call
frames. Ordinary fixtures continue to compare both engines.

## Exercises

1. **Warm-up:** locate the continuation after the example's case in an
   unsimplified ANF dump.
2. **Intermediate:** make two arguments fail with different messages and check
   which must run first for a known call and an unknown call.
3. **Intermediate:** explain why removing an unused division can be wrong even
   though integer division has no IO effects.
4. **Advanced:** instrument the machine on tail-recursive and non-tail-recursive
   sums and compare maximum continuation depth.
5. **Advanced:** extend the deterministic generator with one new well-typed
   construct and an independent expected-result calculation.

## Further reading

- Direct representations and continuation-based compilation [@flanagan1993].
- Join points and shared continuations [@maurer2017].
- The CEK machine's control, environment, and continuation [@felleisen1986].
- Continuation-based compiler construction [@appel1992].
- Polymorphism and type erasure [@pierce2002].
