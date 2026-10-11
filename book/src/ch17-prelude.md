# Chapter 17: The Prelude and Deriving

> **New files**
>
> - `src/derive.rs`
>
> **Changed files**
>
> - `src/primitives.rs`
> - `src/desugar.rs`
> - `src/ast.rs`
> - `src/core_lint.rs`
> - `src/eval.rs`
> - `src/lexer.rs`
> - `src/lib.rs`
> - `src/main.rs`
> - `src/rename.rs`

## Motivation

A language becomes useful when small programs can rely on a coherent library.
That library also tests the compiler unusually well: folds exercise polymorphism,
monadic traversals exercise higher-kinded dictionaries, and derived operations
combine constructor typing, guards, recursion, and instance resolution.

The Prelude stays in Fun source. Deriving extends the same principle: the
compiler writes a conventional instance declaration, then sends it through the
existing checker. There is no separate evaluator shortcut for equality on user
data or for displaying a record. This chapter completes the interpreted language
before the following chapters change its execution machinery.

## The language, now

A datatype may request `deriving Eq` or a nonempty parenthesized list drawn from
`Eq`, `Ord`, and `Show`. The following corpus program deliberately constructs
its record fields in a different order from their declaration:

```fun
{{#include ../../crates/ch17_prelude/tests/programs/prelude/derive-show-record.fun}}
```

Derived equality compares constructors and then their fields. Ordering uses
constructor declaration order, followed by lexicographic field order. Derived
Show prints record fields in declaration order, regardless of construction
order, and inserts parentheses around nested constructor applications where
needed. Lists use comma-separated brackets. `[Char]` is still a list, not a
string. Functions have no derived comparison or display instance.

The completed Prelude provides list folds and traversals, finite generation,
string splitting and joining, numeric conversions, character operations, and
monadic control. `[a..b]` is an inclusive `Int` range. `iterateN` produces a
bounded sequence. Infinite lists are unavailable because constructor fields are
strict. `foldl'` and `foldl` have the same strict behavior here.

`++` now appends lists. Strings use `<>`. This changes 13 inherited fixtures,
all listed in `BREAKING.md`. Numeric literals remain monomorphic. In particular,
`sum` and `product` work on `Int` lists: their identity values are integer
literals, and Fun's `Num` class has no literal-conversion method.

## Theory

Deriving is elaboration into a smaller language. The generated instance must
satisfy exactly the same instance-head, context, superclass, and method checks
as a handwritten one. The Haskell report specifies the structural behavior of
the standard derived instances [@haskell2010]. Fun adapts that behavior to
nominal records, primitive strings, and its smaller `Show` interface.

A parameter used in a field usually introduces a corresponding instance
constraint. A phantom parameter does not. For `data Token a = Token Int`,
equality needs no `Eq a` dictionary because no value of type `a` is examined.
Type synonyms must be expanded before deciding which parameters occur in
fields. The normal instance solver then checks whether the generated context
actually suffices for every field operation [@jones1999]. Deriving is not an
escape from those checks.

Precedence-sensitive display needs more information than `a -> String`. A
helper receives the surrounding precedence and can parenthesize an application
when embedded as an argument. Fun's Show dictionary therefore has an internal
`_showsPrec` method. Handwritten instances inherit a default. Derived instances
provide the structural implementation. Top-level `show` starts with precedence
zero, while positional constructor fields are rendered at precedence eleven.
Record field values use ordinary `show` inside their labelled braces.

Strictness changes another familiar library definition. An ordinary top-level
`undefined = error "undefined"` would fail during startup, even if unused.
Instead, an unshadowed reference to the Prelude's `undefined` is a scoped
abbreviation for applying a unit-taking helper. This preserves failure at the
use site without introducing a thunk or a lazy binding. The evaluation-strategy
distinction from [Chapter 5](ch05-evaluation.md) remains essential [@pierce2002].

## Implementation

`derive::derive` runs immediately before class collection and lexical renaming.
Generated terms retain the datatype's source span, so a rejected field operation
can point back to the declaration. The pass expands aliases when collecting
used parameters, selects one of three generators, and appends ordinary instance
declarations:

```rust
#![function!("../../crates/ch17_prelude/src/derive.rs", derive)]
```

The builders construct the existing surface expression and pattern types.
Equality generates a clause for each pair of constructors. Unequal constructors
return `False`. Matching constructors compare fields from left to right, stopping
at the first unequal pair:

```rust
#![function!("../../crates/ch17_prelude/src/derive.rs", Builder::eq)]
```

Ordering follows the same constructor-pair structure. For a matching pair it
compares each field, continuing only when the result is `EQ`. Show instead
builds string expressions from constructor names, field labels, punctuation,
and recursive `_showsPrec` calls. These expressions pass through ordinary
renaming, inference, dictionary elaboration, match compilation, and Core lint.
Requesting an unsupported class reports `E0211`. Requesting `Ord` without the
required equality evidence also fails through the existing instance checks.

The library lives in `prelude.fun`, embedded ahead of the user program. Its
private helper references are resolved within that scope, so a user binding
cannot silently change the implementation of a Prelude function. Inclusive
ranges check the upper bound before incrementing: a range ending at the largest
63-bit integer must terminate rather than wrap around.

Character classification and simple case conversion use checked-in Unicode
17.0.0 tables in `unicode.fun`. `tools/unicode.py` records the upstream source
and its digest, and the data license is retained. The generated tables are
ordinary compact Fun strings. General-category predicates and simple case
mappings therefore execute through the language rather than adding a large
set of new primitives. `isDigit` deliberately recognizes ASCII decimal digits.
Not every character with a Unicode numeric classification satisfies it.

The larger Prelude exposed two infrastructure costs. Immutable constructor
field vectors are now shared through `Rc`, avoiding repeated deep copies of
list tails during traversal. Core lint removes newly bound unique names on
scope exit instead of cloning the entire term environment. Neither change
alters source semantics. A literal regression also checks that accepted integer
literals, including negative patterns, normalize to signed 63 bits. That bug fix
was applied to all earlier affected Fun snapshots.

## Running it

```text
$ cd crates/ch17_prelude
$ cargo run -q -- run tests/programs/prelude/derive-show-record.fun --lint
Just (P {x = Left 1, y = 2})
$ cargo run -q -- dump --pass=types tests/programs/prelude/derive-show-record.fun
main : IO ()
```

The output demonstrates both declaration-order record fields and parentheses
around the record constructor beneath `Just`. `--pass=core` shows the generated
instance dictionaries. `--pass=match` shows their compiled structural tests.
The separate `derive-show-tree` and `derive-show-literals` fixtures cover nested
applications and escaping.

The corpus grows to 386 programs, with 57 new cases. Most are small executable
library specifications: empty-list behavior, range endpoints, finite iteration,
Unicode classification, monadic traversals, phantom parameters, synonym fields,
and rejection of unsupported deriving. These tests will accompany every later
back end, so the library's observable formatting becomes part of the compiler's
compatibility contract.

## Exercises

1. **Warm-up:** derive `Eq`, `Ord`, and `Show` for a two-constructor tree and compare
   the result with a handwritten instance.
2. **Intermediate:** add an unused datatype parameter and inspect whether its
   dictionary appears in the derived instance context.
3. **Intermediate:** trace the precedence passed while displaying
   `Just (Left (Just 3))`.
4. **Intermediate:** test a singleton range at the maximum `Int` and explain why
   the order of the bound check and increment matters.
5. **Advanced:** measure constructor-tail copying on a Unicode lookup before and
   after immutable field-vector sharing, without changing the Fun program.

## Further reading

- Standard derived instances and the Prelude [@haskell2010].
- Instance contexts and entailment [@jones1999].
- Classes and derived instances in the Haskell Report [@haskell2010].
- Operational semantics and evaluation strategies [@pierce2002].
