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

How do we compare two lists? We compare their elements, which means we need
to know how to compare those elements. We could pass an equality function into
every list comparison. That works, but we'd soon spend a lot of our time
passing around the same handful of operations.

Type classes let the compiler do that plumbing. We describe the operations in
a class and provide implementations in instances. The compiler works out which
implementation a call needs and passes it along.

The lovely part is that we don't need a mysterious new runtime feature. A
class becomes a record of functions, usually called a _dictionary_. Once we've
made those arguments explicit, our existing Core interpreter can run the result.
In this chapter we'll follow that translation all the way through.

## The Language, Now

Let's define a class and give it an instance for integers.

```fun
{{#include ../../crates/ch14_classes/tests/programs/classes/minimal.fun}}
```

Class parameters may have higher kinds. A method `unwrap :: f a -> a` gives its
class parameter `f` kind `* -> *`. The remaining variable `a` is independently
polymorphic. Classes may have superclasses and default method bodies. Instance
methods accept ordinary equations, guards, and local `where` bindings.

The Prelude now defines `Eq`, `Ord`, `Show`, `Num`, `Fractional`, `Semigroup`, and
`Monoid`. Arithmetic and comparisons resolve to methods. Literals remain
monomorphic. `1` has type `Int`, while `1.0` has type `Double`. Consequently
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
still has no IO, so `run` prints the value of `main`. `++` still appends strings
in this chapter. List `++` belongs to the
complete Prelude chapter. `<>` already supports both strings and lists.

## Theory

The type `Eq a => a -> a -> Bool` says we can compare two values of `a` if
someone supplies equality for `a`. The part before `=>` is the _context_. When
we use a function with this type, inference creates a request for that equality
operation. We call this a _wanted_ constraint.

Some requests have immediate answers, such as the instance for `Int`. Others
remain requests about a type variable. When we generalize a binding, those can
become part of its scheme so callers supply the answer. Constraints involving
an outer variable instead belong to the enclosing scope. Jones gives an
executable account of these operations [@jones1999].

Finding an instance is only half the job. We also need something to pass at
runtime. That is the _evidence_ for a constraint. An `Ord a` dictionary already
contains its `Eq a` superclass dictionary. Given `Eq a`, the list instance can
build a dictionary for `Eq [a]`. Following these steps constructs an ordinary
expression, made from dictionary variables, field selections, and instance
applications. There is no runtime search left to perform.

Signatures are checked with rigid skolems and their context as givens. Signed
bindings may be used polymorphically inside their own bodies. Following the
Haskell report [@haskell2010], type-inference dependencies omit edges to signed
bindings. Runtime dependencies retain those edges, since recursive functions still
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

## Running It

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

Expression annotations also work without enclosing parentheses. There is one
place to be careful. Inside a case guard, parenthesize an annotation so its type
arrow can't be confused with the arrow leading to the alternative's body.

## Exercises

1. **Warm-up.** Inspect the dictionary arguments for equality on `[Maybe Int]`.
2. **Intermediate.** Write an instance whose context is insufficient for its
   superclass, then identify the missing proof.
3. **Intermediate.** Explain why `show Nothing` is ambiguous but
   `show (Nothing :: Maybe Int)` is not.
4. **Advanced.** Compare the type-inference and runtime dependency components
   of the polymorphic-recursion fixture.
5. **Advanced.** Corrupt a superclass dictionary's Core type and add a lint
   regression that does not invoke surface inference.

## Further Reading

- Typing Haskell in Haskell [@jones1999].
- Constructor classes and higher-kinded parameters [@jones1993].
- Classes and instances in the language reference [@haskell2010].
- The Haskell report's binding groups and instance restrictions [@haskell2010].
