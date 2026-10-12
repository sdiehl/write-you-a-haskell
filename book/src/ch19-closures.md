# Chapter 19: Closure Conversion

> **New files**
>
> - `src/closure.rs`
> - `src/flat.rs`
> - `src/flat_machine.rs`
> - `src/flat_lint.rs`
>
> **Changed files**
>
> - `src/lib.rs`
> - `src/main.rs`

## Motivation

A function can outlive the call that created it. That is easy to write in Fun,
but it leaves our back end with a question. Where do the variables from that
old call live now?

Our interpreters answered by storing an environment with each function. We'll
make the same idea explicit in the program we compile. Each lambda becomes a
function in a program-wide table. Its closure carries the values that function
needs from the surrounding scope.

We'll also work out how to call these functions. When we know the target, we
can call it directly. When a function arrives as an argument or comes back from
another call, we need to inspect its closure. Partial application adds one more
interesting case. We'll model all of this in a Flat interpreter before we start
laying out the objects in native memory.

## The Language, Now

Here's a small function that remembers `x` and can be passed to `map`.

```fun
{{#include ../../crates/ch19_closure/tests/programs/closure/capture-return.fun}}
```

Every call of the returned function needs the supplied value `20`. Here ANF
uncurrying merges the adjacent lambdas into one global function, so its partial
application retains that value in a PAP. The overloaded addition also needs a
`Num` dictionary. Local lambdas separated by a binding or branch instead require
explicit captured environments. Partial application, oversaturated application,
recursive local functions, and class dictionaries used as values must all keep
working. `--engine=flat` is now the default. Core and ANF remain selectable.

A closure environment is compiler-created storage. The source language gains
no mutable field or reference operation. The only mutation introduced here
initializes capture slots for a recursive group before user code can observe
the closures.

## Theory

Free-variable analysis identifies the values a lambda needs beyond its own
parameters. Globals need no local capture slot because the program can address
them directly. For each remaining free variable, conversion chooses a stable
slot, stores the value when constructing the closure, and replaces lexical
access with an environment load in the lifted function.

Closure conversion and lambda lifting are related but different choices.
Closure conversion packages captured values into an environment. Lambda lifting
can instead add them as explicit function parameters when call sites permit it
[@appel1992]. This chapter keeps one environment mechanism for local functions,
even non-escaping ones. Optional lambda lifting is deferred. A known local call
can still bypass general apply while passing its environment explicitly.

A source function can be called a little at a time. Our lifted machine function
expects a fixed number of arguments. How do we reconcile those two views?

If the function's syntactic arity is `n` and we have fewer arguments, we save
them in a _partial application_, or PAP. If we have exactly `n`, we enter the
function. If we have more, we call it with `n` and apply the leftover arguments
to the function it returns. That's the eval/apply protocol in miniature
[@marlow2004]. A single calling mechanism now handles all three cases.

Mutual recursion gives us a chicken-and-egg problem. If `even` captures `odd`
and `odd` captures `even`, which closure can we build first? We'll allocate both
with placeholder capture slots, then fill the slots once both addresses exist.
No source code can call them until this initialization is finished. Requiring
recursive bindings to be functions makes this possible without trying to
evaluate an unfinished recursive value.

## Implementation

Flat IR removes lambda expressions from executable bodies. Each lifted function
records its parameters, optional environment parameter, body, and ordered capture
list:

```rust
#![struct!("../../crates/ch19_closure/src/flat.rs", Func)]
```

`closure::convert` collects lambda definitions, computes free variables, and
analyzes uses. Capture order follows unique-name order, so dumps and slot indexes
are deterministic. Top-level function entries have no environment parameter.
Lifted local entries do, even when their environment is empty. Prefix
`EnvField` bindings load captures at the start of each local function.

A call is known only when its argument count matches the syntactic arity and
its head is a global function or a non-escaping local lambda. Merely having a
function type is insufficient. The actual decision is small enough to inspect:

```rust
#![function!("../../crates/ch19_closure/src/closure.rs", Convert::known)]
```

Escape analysis regards saturated use as a callee differently from returning,
storing, or passing the function as an argument. A known local call still carries
its closure as an environment operand. Other calls use `CallUnknown`. Tail
position selects the corresponding tail-call form. Partial application therefore
continues through apply even when the source spelling names a familiar function.

The right-hand-side language makes allocation and access visible:

```rust
#![enum!("../../crates/ch19_closure/src/flat.rs", Rhs)]
```

For a `LetRec`, conversion emits every `AllocClosure` before any `PatchClosure`.
The initial capture slots contain placeholder integer literals in the
IR. After patching, each contains its actual captured value. Patch operations
return unit into unused temporary binders. All patches precede the converted
body, so no source expression can call a partially initialized closure.

Flat lint checks each lifted function independently. It rejects an implicit
free variable, unknown function identity, incorrect known-call arity, missing
or unexpected environment operand, out-of-bounds environment load, or invalid
join transfer. Capture binder identities can recur in different functions, since
each environment load rebinds the name in a separate function scope.

The Flat machine owns an indexed arena of closure records. A closure value is
an arena index. Environment slots contain ordinary interpreter values, including
other closure indexes. That permits recursive cycles without reference-count
cycles. PAPs store a function value and supplied arguments. The arena lives for
one execution and models object relationships. It does not yet model reclamation.

Execution is an iterative loop with separate code, known-call, apply, and return
states. Known calls validate the entry's arity and environment. Apply flattens
PAP arguments, handles short and exact calls, and keeps excess arguments in an
explicit continuation. Joins remain lexical control transfers within a function.
Primitive operands cross a data adapter, but evaluation never delegates a Flat
body to the Core or ANF interpreter. Agreement is meaningful because the three
engines implement control flow independently.

## Running It

```text
$ cd crates/ch19_closure
$ cargo run -q -- run tests/programs/closure/capture-return.fun --lint
[21,22,23]
$ cargo run -q -- dump --pass=types tests/programs/closure/capture-return.fun
main : IO ()
```

Dump this file with `--pass=flat` to find the partial application of `make`.
The global entry has no environment. Apply saves its dictionary and integer
arguments in a PAP. In contrast, `escape-recursive.fun` returns a local recursive
closure capturing an outer value, and `known-local.fun` uses direct local calls. `pap-chain.fun` supplies arguments in
stages. `over-application.fun` supplies arguments to a function whose result is
itself a function. The small Flat snapshots in `tests/anf.rs` omit unrelated
Prelude functions by selecting reachable code.

Try supplying arguments in stages, returning a local function, and making two
local functions call each other while both capture an outer value. These all
look natural in source, but exercise different parts of the closure machinery.
Core, ANF, and Flat should agree on their results. The Flat machine also checks
that a long tail-recursive sum keeps a bounded number of pending continuations.

## Exercises

1. **Warm-up.** Trace how uncurrying and partial application preserve the example's
   integer argument and `Num` dictionary.
2. **Intermediate.** Change a local function from saturated-only use to returning
   it as a value. Explain the resulting call classification.
3. **Intermediate.** Trace a PAP receiving another argument, followed by an
   oversaturated call whose first result is a closure.
4. **Advanced.** Construct mutually recursive local functions that also capture
   an outer integer. Draw their environment graph after patching.
5. **Advanced.** Prototype lambda lifting for non-escaping functions and compare
   allocation counts while retaining differential agreement.

## Further Reading

- Eval/apply, partial application, and calling conventions [@marlow2004].
- Closure conversion in a complete compiler [@appel1992].
- Strict functional implementation and value representation [@leroy1990].
- Local joins versus first-class continuations [@maurer2017].
