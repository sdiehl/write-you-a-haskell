# Chapter 10: Typed Core and Core Lint

> **New files**
>
> - `src/core_lint.rs`
> - `src/core_type.rs`
>
> **Changed files**
>
> - `src/core.rs`
> - `src/elaborate.rs`
> - `src/typecheck.rs`
> - `src/eval.rs`
> - `src/lib.rs`
> - `src/main.rs`
> - `src/rename.rs`
> - `src/renamed.rs`

## Motivation

The type checker has accepted our program. Excellent. Now the compiler gets
to rewrite it, which gives us plenty of opportunities to introduce mistakes of
our own. We might pass the wrong argument, forget a type instantiation, or reuse
a variable identity. The source program being correct doesn't help if we
translate it incorrectly.

We'll give Core explicit types and write a second checker for it. This checker
has an easier job than inference. All the answers are already in the tree, so
it only needs to verify that they fit together. When a transformation goes
wrong, we can catch it close to where it happened. A compiler that checks its
own homework is a rather useful thing to have.

## The Language, Now

We can still write polymorphic functions without spelling out their type arguments.

```fun
id x = x
number = id 42
main = id True
```

In Core, the definition of `id` abstracts over a type variable. Its two uses apply
it first to `Int` and then to `Bool`. Those applications are real IR nodes rather
than comments on an untyped expression. The evaluator ignores their types, so this
program still prints `True`.

## Theory

The source expression `id 42` hides a small decision. We're using identity at
`Int`. System F gives us a way to write that decision into the program. A
_type abstraction_ introduces a type parameter, and a _type application_ chooses
a type for it. Girard and Reynolds developed this explicit treatment of
polymorphism [@girard1972; @reynolds1974].

Think of the two new forms alongside ordinary lambdas and calls. An ordinary
lambda waits for a value. A type abstraction abstracts over a type. This analogy
helps us read the rules, although the type arguments will disappear before
runtime.

$$
\frac{\Gamma, \alpha \vdash e : \tau \quad \alpha \notin \mathsf{ftv}(\Gamma)}{\Gamma \vdash \Lambda\alpha.e : \forall\alpha.\tau}\;\text{(Type abstraction)}
$$

$$
\frac{\Gamma \vdash e : \forall\alpha.\tau \quad \Gamma \vdash \sigma : *}{\Gamma \vdash e[\sigma] : \tau[\sigma/\alpha]}\;\text{(Type application)}
$$

Every type binder has kind `*` in this chapter. Higher-kinded constructors will
extend the kind judgment later. Ordinary application still requires an arrow type.
A universally quantified function must first receive its type arguments.

Core supports full System F. Fun still exposes only HM polymorphism. Hand-built
Core terms test that distinction without extending source syntax [@pierce2002].

## Implementation

### The Core Datatypes

A Core expression is a Rust enum. Each variable and term binder carries an
`Id` with its type, so lint can check the tree without rerunning inference.

```rust
#![struct!("../../crates/ch10_core/src/core.rs", Id)]
```

The expression constructors distinguish term-level functions (`Lam`, `App`)
from type-level abstraction and instantiation (`TyLam`, `TyApp`). `Let` keeps
the recursive components produced by the renamer. `Prim` carries explicit type
arguments, evaluated operands, and a source span for failures.

```rust
#![enum!("../../crates/ch10_core/src/core.rs", Expr)]
```

Binding groups hold typed names and right-hand sides. A program is an ordered
list of these groups together with the entry point:

```rust
#![struct!("../../crates/ch10_core/src/core.rs", Bind)]
#![struct!("../../crates/ch10_core/src/core.rs", BindGroup)]
#![struct!("../../crates/ch10_core/src/core.rs", Program)]
```

This chapter still has `If`. Datatype constructors and pattern matches arrive
in Chapter 11, and flat cases and join points arrive in Chapter 16. The smaller
enum here lets us establish typed elaboration and lint before adding those forms.

### Recording inference results

Renamed expressions now have stable node numbers. The checker records the fresh
type arguments used at each variable occurrence, the scheme of each binding, and
the monotype of each lambda parameter. Before returning, it zonks all these types,
so elaboration never needs access to the mutable metavariable store.

A variable occurrence inferred inside a recursive group initially sees a provisional
monotype. Once the component is generalized, elaboration instantiates its recursive
Core references at the corresponding generalized variables. Ordinary occurrences
use the fresh arguments recorded during inference.

### Elaborating polymorphism

Each generalized metavariable receives a fresh Core type identity. Binder types
wrap the converted monotype in `Forall`. Right-hand sides wrap their terms in the
matching `TyLam`s. Local schemes can still mention type variables bound by an
outer function's abstraction.

```rust
{{#include ../../crates/ch10_core/src/elaborate.rs:generalize-core}}
```

Occurrences gain `TyApp`s, and lambda parameters carry their converted types.
Core distinguishes global references from local variables. Its binder annotations
are checked against the declared environment, so attaching a plausible-looking
type to an otherwise wrong occurrence cannot evade lint.

An internal variable absent from every enclosing scheme needs a closed witness:
`(\f -> 42) (\x -> x)` is an example. We use `Int` for these erased annotations.
This does not default source expressions or resolve ambiguous class constraints.

### Independent lint

The checker maintains term and type scopes, plus sets of binder identities already
used. It checks arrow applications, primitive saturation, conditional branches,
recursive function groups, and binder annotations. These are the explicit type
abstraction and application rules:

```rust
{{#include ../../crates/ch10_core/src/core_lint.rs:type-rules}}
```

Type application substitutes into the quantified body. Substitution must avoid
capturing a free type variable in the argument, even for Core terms constructed
outside our elaborator. The implementation freshens a conflicting binder:

```rust
{{#include ../../crates/ch10_core/src/core_type.rs:type-substitution}}
```

Type equality is alpha equivalence, so two `forall` types may use different fresh
identities and still denote the same type. In contrast, a free type variable must
refer to the same enclosing binder.

```rust
{{#include ../../crates/ch10_core/src/core_lint.rs:core-lint}}
```

> **Aside.** A compiler bug that drops `TyApp` from an application of polymorphic
> identity produces `Core application of non-function type forall t0. t0 -> t0`.
> The source program may be valid. The generated Core is not. The malformed-Core
> tests check this failure directly, without rerunning source inference.

### Erasure and strict execution

`TyLam` and `TyApp` do not allocate runtime objects. The interpreter steps through
them without substituting runtime values. This is erasure, rather than a separate
runtime evaluation strategy for System F.

In particular, a type abstraction does not delay a strict initializer. The unused
polymorphic binding below still fails before `main` is selected:

```fun
unused = (\x -> \y -> y) (div 1 0)
main = 42
```

Chapter 18 introduces an explicit erased IR. Corpus runs already enable `--lint`.

## Running It

```text
$ cd crates/ch10_core
$ cargo run -q -- run tests/programs/core/self-instantiation.fun --lint
42
$ cargo run -q -- dump --pass=core --no-prelude tests/programs/layout/signature.fun
nonrec { id#0 : forall t0. t0 -> t0 = (type-lambda t0 (lambda x#2 : t0 x#2)); }
nonrec { main#1 : Int = (app (type-app @id#0 Int) 42); }
$ cargo test -p ch10_core --test system_f
```

For a useful experiment, remove a type application from a small Core term
and run lint. The source checker has no role in this test. We're deliberately
feeding the second checker a mistake that only the compiler could have made.
It should reject the term before evaluation.

## Exercises

1. **Introductory.** identify every type abstraction and type application in the
   Core dump for `id id 42`.
2. **Intermediate.** Explain why a captured outer type variable remains free in a
   local binding's scheme but is bound in the complete Core program.
3. **Intermediate.** Construct a malformed Core primitive call with the right
   number of arguments but the wrong operand type.
4. **Advanced.** Demonstrate a capture bug in type substitution if freshening is
   removed, then explain why alpha equivalence alone does not repair it.

## Further Reading

Girard's thesis and Reynolds's paper are the original sources for explicit
polymorphism [@girard1972; @reynolds1974]. Pierce develops System F's typing and
substitution rules [@pierce2002].
