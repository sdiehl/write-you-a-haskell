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

An ANF lambda can refer to names in its surrounding lexical scope. A machine
function cannot rely on that compiler environment still existing when the
function is called. If a function escapes its defining activation, the values
it needs must travel with it.

Closure conversion makes that storage explicit. Each lambda becomes a function
in a program-wide table, and each closure packages its function identity with
captured values. Calls with a known target can use the function directly.
Other calls inspect the closure through a common application protocol. An
independent Flat interpreter lets us validate that protocol before committing
to native memory layouts.

## The language, now

No syntax changes. This example returns a function that remembers `x`, then
passes it to `map`:

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

Currying requires an application protocol. If a closure's syntactic arity is
`n`, an application with fewer arguments returns a partial application, or PAP.
An exact application enters the function. An application with excess arguments
first calls the function with `n`, then applies the remaining arguments to the
result. Marlow and Peyton Jones describe this eval/apply organization and its
relationship to other calling conventions [@marlow2004]. Fun uses a deliberately
small version suitable for its strict language.

Recursive groups add an allocation dependency. If `even` captures `odd` and
`odd` captures `even`, neither final environment can be constructed first.
Allocate all closures with placeholder slots, then patch the slots once every
closure has an identity. The source restriction to recursive functions ensures
that this initialization need not evaluate a recursive non-function value.

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
join transfer. Capture binder identities can recur in different functions:
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

## Running it

```text
$ cd crates/ch19_closure
$ cargo run -q -- run tests/programs/closure/capture-return.fun --lint
[21,22,23]
$ cargo run -q -- dump --pass=types tests/programs/closure/capture-return.fun
main : IO ()
```

Dump this file with `--pass=flat` to find the partial application of `make`.
The global entry has no environment: apply saves its dictionary and integer
arguments in a PAP. In contrast, `escape-recursive.fun` returns a local recursive
closure capturing an outer value, and `known-local.fun` uses direct local calls. `pap-chain.fun` supplies arguments in
stages. `over-application.fun` supplies arguments to a function whose result is
itself a function. The small Flat snapshots in `tests/anf.rs` omit unrelated
Prelude functions by selecting reachable code.

The corpus grows to 402 programs, adding eight closure-focused cases. All three
engines run the eligible inherited programs and 1,000 generated integer cases.
Snapshots also compare simplification enabled and disabled. A million-call
Flat sum returns `500000500000` with bounded continuation depth, while the
inherited ANF one-million-call check remains in place. Cyclic local environments,
dictionary captures, shadowed names, and escaping recursive closures have
separate regressions.

## Exercises

1. **Warm-up:** trace how uncurrying and partial application preserve the example's
   integer argument and `Num` dictionary.
2. **Intermediate:** change a local function from saturated-only use to returning
   it as a value. Explain the resulting call classification.
3. **Intermediate:** trace a PAP receiving another argument, followed by an
   oversaturated call whose first result is a closure.
4. **Advanced:** construct mutually recursive local functions that also capture
   an outer integer. Draw their environment graph after patching.
5. **Advanced:** prototype lambda lifting for non-escaping functions and compare
   allocation counts while retaining differential agreement.

## Further reading

- Eval/apply, partial application, and calling conventions [@marlow2004].
- Closure conversion in a complete compiler [@appel1992].
- Strict functional implementation and value representation [@leroy1990].
- Local joins versus first-class continuations [@maurer2017].
