# Chapter 13: Kinds and Type Constructors

> **New files**
>
> - `src/kinds.rs`
>
> **Changed files**
>
> - `src/types.rs`
> - `src/typecheck.rs`
> - `src/core_lint.rs`
> - `src/ast.rs`
> - `src/core.rs`
> - `src/core_type.rs`
> - `src/data.rs`
> - `src/elaborate.rs`
> - `src/lexer.rs`
> - `src/lib.rs`
> - `src/main.rs`
> - `src/parse.rs`
> - `src/rename.rs`

## Motivation

`Maybe Int` is a type of values, while `Maybe` takes a type before it describes
values. Chapter 11 checked this distinction by counting arguments. That works
until a datatype parameter itself takes arguments. A wrapper around `f a` needs
to express the difference between the roles of `f` and `a`.

Kinds describe those roles. They use a small language of arrows and `*`, where
`*` denotes a type of values. The same unification ideas used for types work for
kinds, with a separate store and no interaction between their metavariables.
This prepares the type constructor parameters used by classes in Chapter 14.

## The language, now

```fun
data Wrap f a = Wrap (f a)

unwrap :: Wrap Maybe Int -> Maybe Int
unwrap (Wrap x) = x

main = unwrap (Wrap (Just 42))
```

`Wrap` has kind `(* -> *) -> * -> *`. A recursive declaration such as
`data Fix f = Fix (f (Fix f))` gives `Fix` kind `(* -> *) -> *`. Declaration
dependencies are grouped into strongly connected components. Each component
is inferred before its unconstrained kind variables default to `*`.

```fun
type Pair a = (a,a)
type Count = Int

addPair :: Pair Count -> Count
addPair (x,y) = x + y
main = addPair (20,22)
```

A synonym introduces no constructor or runtime representation. It must be fully
applied, even where a partially applied datatype would have the right kind.
Cycles are rejected, including unused cycles. `E0301` reports a kind mismatch,
`E0302` a partial synonym, and `E0303` a synonym cycle. The type constructors
`[]`, `(,)`, `(,,)`, and `(->)` may appear unapplied where their kinds permit.

## Theory

The application rule consumes one arrow in a constructor's kind:

$$
\frac{\Gamma \vdash f : \kappa \to \sigma \quad \Gamma \vdash a : \kappa}
{\Gamma \vdash f\ a : \sigma}\;\text{(Application)}
$$

Function arguments and results must each have kind `*`. A constructor field must
also have kind `*`: a value cannot inhabit the unapplied type constructor
`Maybe`. The distinction between kinds and types follows the presentation of
higher-order type operators in Pierce [@pierce2002]. Fun infers kinds but does
not add type-level lambdas or explicit kind annotations.

Synonym substitution follows the Haskell report's fully applied convention
[@haskell2010]. Expanding arguments before entering a synonym permits
`Id (Id Int)` without mistaking the repeated name for a declaration cycle.
A stack of definitions detects actual recursion during expansion.

## Implementation

Kinds classify type constructors in the same way that types classify terms.
The implementation combines kind unification, an occurs check, and dependency
ordering to infer those classifications.

```rust
{{#include ../../crates/ch13_kinds/src/kinds.rs:kind}}
```

`Store::unify` follows solutions, compares arrow structure, and rejects a
metavariable occurring inside its own proposed solution. `finish` defaults
unsolved cells only after all fields in a declaration component were checked.

```rust
{{#include ../../crates/ch13_kinds/src/kinds.rs:kind-unify}}
```

The declaration pass first installs every head, then reuses Chapter 9's Tarjan
implementation. Signatures infer implicit variable kinds against that closed
table. The immutable datatype environment validates synonym use and converts
the expanded syntax to inference types.

The type store now records a kind on each metavariable. Instantiating a scheme
copies its quantified kinds as well as freshening its variables. Unification of
`f a` with `Maybe Int` separates the final application from each side, solving
`f = Maybe` and `a = Int` without requiring either to be a value type.

Core keeps a kind table keyed by unique type binder identities. Lint independently
checks applications, constructor arguments, and function operands against it.
Internal type variables absent from every enclosing scheme receive a closed
witness of the right kind. These compiler-only type constants have no values or
source syntax and disappear at erasure. This extends the `Int` witness used in
Chapter 10 to kinds for which `Int` is unsuitable.

## Running it

```text
$ cd crates/ch13_kinds
$ cargo run -q -- run tests/programs/kinds/wrapper.fun --lint
Just 42
$ cargo run -q -- dump --pass=types tests/programs/kinds/wrapper.fun
main : Maybe Int
$ cargo run -q -- check tests/programs/kinds/synonym-self.fun
E0303
```

The last command exits unsuccessfully and also prints a labelled diagnostic.
The cumulative corpus retains Chapter 12's behavior except for its deliberate
higher-kinded-parameter rejection, which now succeeds. New tests include partial
constructors, recursive fixed-point datatypes, records with higher-kinded
parameters, malformed Core kinds, and synonym failures.

## Exercises

1. **Warm-up:** infer the kind of `data Compose f g a = Compose (f (g a))`.
2. **Intermediate:** explain why `data Bad f = Bad (f f)` fails the occurs check.
3. **Intermediate:** trace kind inference through mutually dependent declarations.
4. **Advanced:** find a term whose elaboration needs an internal witness of kind
   `* -> *`, and inspect the corresponding Core application.

## Further reading

- Pierce's treatment of type operators [@pierce2002].
- The Haskell report's datatype and synonym rules [@haskell2010].
- Higher-order type operators and System F-omega [@pierce2002].
