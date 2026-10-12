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

Consider `f (g x) (h y)`. To run it, we need to evaluate the function and its
arguments in the right order, keep the result of `g x` while computing `h y`,
and finally make the call. Our interpreter has been doing that bookkeeping
for us. A code generator needs to see the steps.

A-normal form, usually shortened to ANF, writes those intermediate results down
as named bindings. Function calls and other operations then take simple operands
rather than nested computations. The name sounds rather grand for what amounts
to introducing some useful local variables.

This also gives us a good place to bring back the CEK machine from Chapter 5.
We'll run ANF with an explicit continuation stack, now with all of Fun's data,
dictionaries, IO, and join points. We can compare its answers with Core while
getting the program ready for native code.

## The Language, Now

Let's follow a case expression whose result is used by another computation.

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

In `f (g x) (h y)`, the interesting work is in the calls. The names themselves
just refer to values. ANF makes that distinction part of the syntax. An _atom_
is a literal or a reference to an available value. A right-hand side performs
one operation on atoms, and a `Let` gives the result a name. Nested evaluation
has become an explicit sequence [@flanagan1993].

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
program. Pending non-tail work can still grow the explicit continuation stack.
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

### The ANF Grammar

Here is the control language in symbolic form. We use $a$ for an atom, $r$ for
a right-hand side, and $e$ for a complete ANF body. Each row corresponds to a
Rust variant below.

$$
\begin{aligned}
e & = \text{let}\;x = r\;\text{in}\;e & \quad & \text{strict binding} \\
  & \mid \text{letrec}\;\{\overline{x = \lambda\overline{y}.\;e_1}\}\;\text{in}\;e_2 & \quad & \text{recursive functions} \\
  & \mid \text{join}\;j(\overline{x}) = e_1\;\text{in}\;e_2 & \quad & \text{local continuation} \\
  & \mid \text{jump}\;j(\overline{a}) & \quad & \text{jump to a join} \\
  & \mid \text{case}\;a\;\text{of}\;\{\overline{q\;\overline{x}\to e}\} & \quad & \text{branch on an atom} \\
  & \mid \text{return}\;a & \quad & \text{return a value} \\
  & \mid \text{tailcall}\;a(\overline{a}) & \quad & \text{replace this activation} \\
  & \mid \text{fail}_{s} & \quad & \text{pattern failure at span }s
\end{aligned}
$$

An overbar means a sequence. The name $j$ identifies a join point, while $q$
is an alternative's constructor tag, literal, or default marker. Constructor
alternatives bind their fields to $\overline{x}$. Literal and default
alternatives have no field binders.

The smaller pieces explain the “normal form” part of the name. An operand is
always an atom, never another call waiting to happen.

$$
\begin{aligned}
a & = x & \quad & \text{local variable} \\
  & \mid g & \quad & \text{global reference} \\
  & \mid \ell & \quad & \text{literal} \\
r & = a & \quad & \text{atom} \\
  & \mid a(\overline{a}) & \quad & \text{function call} \\
  & \mid K(\overline{a}) & \quad & \text{constructor} \\
  & \mid p(\overline{a}) & \quad & \text{primitive operation} \\
  & \mid \lambda\overline{x}.\;e & \quad & \text{function value} \\
  & \mid \text{field}\;a\;i & \quad & \text{field at index }i
\end{aligned}
$$

Here $K$ names a constructor and $p$ a primitive. Primitive source spans are
omitted from the grammar. Unlike Core's lambdas, these lambdas can take several
parameters, and there are no type abstractions or type applications left.

The distinction between a call and a tail call is now visible in the syntax.
$\mathsf{let}\;x = f(a)\;\mathsf{in}\;e$ saves work to do after the call.
$\mathsf{tailcall}\;f(a)$ has no such body. A jump also has no following body,
but stays inside the current function and transfers to a local join point.

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

## Running It

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

Alongside the handwritten examples, `testkit/src/gen.rs` generates small
well-typed programs containing arithmetic, local bindings, branches, recursion,
and lists. Both engines must agree with an independent wrapping-integer
calculation. We save the source and seed of a failure so an unexpected answer
becomes a program we can debug.

A million-call sum is a useful test of the machine's control flow. We measure
the number of pending continuations, rather than just checking that it finishes.
A tail call should replace the current computation. A non-tail call may add
pending work to the heap-allocated continuation stack. Neither needs another
Rust call frame. Core uses its own explicit machine, so comparing it with ANF
also checks that conversion hasn't changed the answer.

## Exercises

1. **Warm-up.** Locate the continuation after the example's case in an
   unsimplified ANF dump.
2. **Intermediate.** Make two arguments fail with different messages and check
   which must run first for a known call and an unknown call.
3. **Intermediate.** Explain why removing an unused division can be wrong even
   though integer division has no IO effects.
4. **Advanced.** Instrument the machine on tail-recursive and non-tail-recursive
   sums and compare maximum continuation depth.
5. **Advanced.** Extend the deterministic generator with one new well-typed
   construct and an independent expected-result calculation.

## Further Reading

- Direct representations and continuation-based compilation [@flanagan1993].
- Join points and shared continuations [@maurer2017].
- The CEK machine's control, environment, and continuation [@felleisen1986].
- Continuation-based compiler construction [@appel1992].
- Polymorphism and type erasure [@pierce2002].
