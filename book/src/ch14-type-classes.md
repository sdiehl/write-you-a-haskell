# Chapter 14: Type Classes

> **New files**
>
> - `src/classes.rs`
> - `src/evidence.rs`
> - `src/elaborate/dictionaries.rs`
> - `src/primitives.rs`
>
> **Changed files**
>
> - `src/typecheck.rs`
> - `src/elaborate.rs`
> - `src/types.rs`
> - `src/ast.rs`
> - `src/core.rs`
> - `src/core_lint.rs`
> - `src/core_type.rs`
> - `src/data.rs`
> - `src/desugar.rs`
> - `src/eval.rs`
> - `src/fixity.rs`
> - `src/kinds.rs`
> - `src/lexer.rs`
> - `src/lib.rs`
> - `src/main.rs`
> - `src/parse.rs`
> - `src/rename.rs`
> - `src/renamed.rs`
> - `src/syn.rs`

## Motivation

An equality function for integers cannot compare a list without knowing how to
compare its elements. Passing that operation explicitly works, but repeats the
same plumbing at every call. A type class names a set of operations, and an
instance supplies their implementation for a particular type constructor.

This chapter makes that plumbing the compiler's responsibility. Inference records
which operations a term needs. Elaboration passes ordinary records of functions.
The Core interpreter needs no class lookup. Independent lint checks the resulting
constructor fields, type applications, and function arguments just as it checked
ordinary programs in earlier chapters.

## The language, now

This complete corpus program defines a class and its integer instance:

```fun
{{#include ../../crates/ch14_classes/tests/programs/classes/minimal.fun}}
```

Class parameters may have higher kinds. A method `unwrap :: f a -> a` gives its
class parameter `f` kind `* -> *`. The remaining variable `a` is independently
polymorphic. Classes may have superclasses and default method bodies. Instance
methods accept ordinary equations, guards, and local `where` bindings.

The Prelude now defines `Eq`, `Ord`, `Show`, `Num`, `Fractional`, `Semigroup`, and
`Monoid`. Arithmetic and comparisons resolve to methods. Literals remain
monomorphic: `1` has type `Int`, while `1.0` has type `Double`. Consequently
`1 + 2.0` fails. `toDouble 1 + 2.0` succeeds. There is no defaulting.

```fun
same :: Ord a => a -> a -> Bool
same x y = x == y

main = same [1,2] [1,2]
```

`Ord` provides an `Eq` superclass, so the signature supplies enough evidence.
An absent instance reports `E0210`. Malformed or overlapping instances and missing
methods report `E0211`. A constraint whose variable is absent from the result
type reports `E0230`: for example, `show Nothing` cannot choose the element's
`Show` instance. An explicit `Maybe Int` annotation resolves that ambiguity.

`Double` adds floating arithmetic, comparisons, conversions, and mathematical
functions. Primitive names remain private to the embedded Prelude. The source
still has no IO: `run` prints the value of `main`. For compatibility with the
inherited corpus, `++` remains string append here. List `++` belongs to the
complete Prelude chapter. `<>` already supports both strings and lists.

## Theory

A qualified scheme consists of quantified variables, a context of predicates,
and a type. Instantiation freshens its variables and creates one wanted predicate
per context entry. Generalization reduces concrete predicates using instances,
simplifies superclass consequences, and retains predicates over the generalized
variables. Constraints concerning an outer variable float to its enclosing group.
Jones gives an executable presentation of these constraint operations
[@jones1999].

Evidence gives the typing judgment an operational meaning. If a context contains
an `Ord a` dictionary, its superclass field supplies an `Eq a` dictionary. If
it contains `Eq a`, the list instance constructs evidence for `Eq [a]`. A wanted
predicate is solved by a given, a superclass projection, or an instance applied
to recursively solved context predicates.

Signatures are checked with rigid skolems and their context as givens. Signed
bindings may be used polymorphically inside their own bodies. Following the
Haskell report [@haskell2010], type-inference dependencies omit edges to signed
bindings. Runtime dependencies retain those edges: recursive functions still
need a shared recursive environment. These are two different uses of the same
source dependency graph.

Generalization remains unrestricted. A constrained value such as `comparison =
(==)` elaborates to a function taking a dictionary. Its body is evaluated when
that dictionary is supplied, so source-level sharing is lost. Fun accepts this
consequence of dictionary passing. Purity makes it sound.

## Implementation

`classes.rs` implements superclass search, instance matching, and entailment.
The main inferencer continues to use mutable, level-based metavariables. The small
immutable map used to match an instance head is local to that lookup. It is not
an alternative substitution-based type checker.

```rust
{{#include ../../crates/ch14_classes/src/classes.rs:evidence}}
```

A given stores its position in the lexical dictionary environment and a path
through superclass fields. An instance proof stores the selected instance,
its type arguments, and proofs of its prerequisites.

```rust
{{#include ../../crates/ch14_classes/src/classes.rs:entailment}}
```

Class validation checks superclass cycles, method membership, extra method
constraints, distinct variables in nominal instance heads, overlap, and
superclass availability. Kind inference processes superclass dependencies before
defaulting unconstrained class kinds. Instance contexts may mention only the
head's variables. Solving therefore descends into structurally smaller types.

Field constraints still run before class constraints at generalization. After
zonking, `evidence.rs` records the class proofs at variable occurrences while
their lexical givens are available. Elaboration consumes these checked proofs.

```rust
{{#include ../../crates/ch14_classes/src/evidence.rs:evidence-variable}}
```

`elaborate/dictionaries.rs` declares one nominal Core dictionary type per class.
Its fields contain superclass dictionaries followed by methods. Method-local
quantifiers become System F polymorphism inside the field type. This does not
add rank-N signatures to the source language. Each instance becomes a global
value, abstracted over its type variables and context dictionaries. Method
selectors are ordinary matches on the dictionary constructor.

```rust
{{#include ../../crates/ch14_classes/src/elaborate/dictionaries.rs:dictionary-type}}
```

Defaults get named globals, and each missing implementation is checked from a
copy of its default body specialized to the instance. This keeps strict
dictionary construction from tying a recursive value knot. Dictionary-introduced
dependencies are ordered before evaluation. Recursive non-function initialization
is rejected. The evaluator installs function closures before evaluating values,
and each unconstrained top-level value is still evaluated once.

`primitives.rs` supplies fixed primitive signatures and implementations. Integer
operations retain 63-bit wrapping. Floating display uses shortest round-trip
digits with the specified positional range, signed exponents, and explicit
special values. Tests cover negative zero, boundary magnitudes, Unicode strings,
and failed conversions. The shared runtime chapter will move the non-trivial
operations into `fun-rt`.

## Running it

```text
$ cd crates/ch14_classes
$ cargo run -q -- run tests/programs/classes/minimal.fun --lint
42
$ cargo run -q -- dump --pass=types tests/programs/classes/principal-type.fun
main : Num a => a -> a -> a
$ cargo run -q -- check tests/programs/classes/mixed-literals.fun
E0201
```

The failing command also prints a labelled diagnostic. `--dump=core` exposes
`$D` dictionary types, `$f` instance globals, and explicit dictionary applications.
The small dictionary snapshots disable the Prelude to keep those examples
readable. Ordinary builds compile the embedded Prelude before the user's scope.
Shadowing a Prelude name cannot change the Prelude's own references.

All inherited programs retain their expectations. New tests cover default
methods, superclass chains, higher-kinded classes, polymorphic method fields,
qualified annotations, polymorphic recursion, ambiguity, and malformed Core.
Expression annotations are supported without enclosing parentheses. A case
alternative's guard needs parentheses around an annotation to distinguish its
type arrow from the alternative arrow.

## Exercises

1. **Warm-up:** inspect the dictionary arguments for equality on `[Maybe Int]`.
2. **Intermediate:** write an instance whose context is insufficient for its
   superclass, then identify the missing proof.
3. **Intermediate:** explain why `show Nothing` is ambiguous but
   `show (Nothing :: Maybe Int)` is not.
4. **Advanced:** compare the type-inference and runtime dependency components
   of the polymorphic-recursion fixture.
5. **Advanced:** corrupt a superclass dictionary's Core type and add a lint
   regression that does not invoke surface inference.

## Further reading

- Typing Haskell in Haskell [@jones1999].
- Constructor classes and higher-kinded parameters [@jones1993].
- Classes and instances in the language reference [@haskell2010].
- The Haskell report's binding groups and instance restrictions [@haskell2010].
