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

A language starts to feel like a language when you can reach for `map`, `foldr`,
or `maybe` without writing them yourself every time. We now have enough Fun to
write its standard library in Fun. That's a good milestone!

We'll also teach the compiler to write the repetitive parts. Given a datatype,
`deriving` generates an ordinary instance for equality, ordering, or display.
The generated code goes through the same checker as code we'd write by hand.
There's no secret equality operation in the interpreter waiting to rescue it.

This chapter brings the interpreted language together. Take a little time to
play with it before we head into the back end. Small library functions are also
excellent compiler examples. Their types and definitions often tell the whole
story in just a few lines.

## The Language, Now

A datatype may request `deriving Eq` or a nonempty parenthesized list drawn from
`Eq`, `Ord`, and `Show`. This example constructs
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

Use `++` to append lists and `<>` to append strings. Numeric literals still
have fixed types, so `sum` and `product` operate on `Int` lists. Their starting
values are integer literals, and Fun's `Num` class has no operation for turning
a literal into an arbitrary numeric type.

### Putting Small Functions Together

Now we can write something compact enough to read in one breath.

```fun
total = sum . map (\x -> x * x) . filter even

main = print (total [1..10])
```

This prints `220`. Read the composition from right to left. Keep the even
numbers, square each one, then add them up. Each function does one familiar job,
and `(.)` connects the result of one to the input of the next. We don't have to
name a loop index or describe how to advance through the list.

That is a beautiful way to express the calculation. It doesn't mean the work
has vanished. Fun is strict, and this program builds intermediate lists. Later
optimizations could remove them, but the useful separation is already here.
We can understand what the program means before choosing how to make it faster.

## Theory

Imagine writing equality for a tree by hand. Empty trees are equal. Two nodes
are equal when their corresponding fields are equal. Different constructors
aren't equal. The datatype declaration already tells us enough to write those
clauses, so let's have the compiler write them.

That's deriving. We generate an ordinary instance, then check it just as we
would check a handwritten one. The Haskell Report specifies the structural
behavior [@haskell2010]. We adapt it to Fun's records, primitive strings, and
smaller `Show` interface.

For `data Choice a = Empty | One a`, we could write the essential equality
cases ourselves.

```fun
instance Eq a => Eq (Choice a) where
  (==) Empty Empty = True
  (==) (One x) (One y) = x == y
  (==) _ _ = False
```

The middle clause explains the `Eq a` requirement. We need equality for the
stored values. The other clauses only inspect constructor tags. Deriving
builds these same decisions from the declaration and sends the result through
our ordinary instance checker.

A parameter used in a field usually introduces a corresponding instance
constraint. A phantom parameter does not. For `data Token a = Token Int`,
equality needs no `Eq a` dictionary because no value of type `a` is examined.
Type synonyms must be expanded before deciding which parameters occur in
fields. The normal instance solver then checks whether the generated context
actually suffices for every field operation [@jones1999]. Deriving is not an
escape from those checks.

Printing constructors has a small but important wrinkle. `Just (Left 3)` needs
parentheses, but `Left 3` on its own doesn't. A function of type `a -> String`
can't see where its result will be used. We need to pass that surrounding
precedence in as well.

Fun's Show dictionary has an internal `_showsPrec` method for this purpose.
Handwritten instances inherit a default, while derived instances implement it
structurally. Top-level `show` starts at precedence zero. Positional constructor
arguments use eleven, which asks nested constructor applications to parenthesize
themselves. Record fields use ordinary `show` inside their labelled braces.

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
ranges check the upper bound before incrementing. A range ending at the largest
63-bit integer must terminate rather than wrap around.

Character classification and simple case conversion use checked-in Unicode
17.0.0 tables in `unicode.fun`. `tools/unicode.py` records the upstream source
and its digest, and the data license is retained. The generated tables are
ordinary compact Fun strings. General-category predicates and simple case
mappings therefore execute through the language rather than adding a large
set of new primitives. `isDigit` deliberately recognizes ASCII decimal digits.
Not every character with a Unicode numeric classification satisfies it.

A library also teaches us where the compiler is doing unnecessary work. Walking
a list shouldn't copy its entire tail at every step. We share immutable
constructor field vectors with `Rc`, and Core lint removes local bindings on
scope exit instead of copying its whole environment. These are useful reminders
to look at the cost of our Rust representation as well as the Fun program.

## Running It

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

Library edge cases make good experiments. Try a singleton range at the largest
`Int`, an empty list passed to a fold, or a derived instance with a phantom
parameter. The little examples are doing real work here. A boundary case can
explain a library definition more clearly than a long ordinary input.

## Exercises

1. **Warm-up.** Derive `Eq`, `Ord`, and `Show` for a two-constructor tree and compare
   the result with a handwritten instance.
2. **Intermediate.** Add an unused datatype parameter and inspect whether its
   dictionary appears in the derived instance context.
3. **Intermediate.** Trace the precedence passed while displaying
   `Just (Left (Just 3))`.
4. **Intermediate.** Test a singleton range at the maximum `Int` and explain why
   the order of the bound check and increment matters.
5. **Advanced.** Measure constructor-tail copying on a Unicode lookup before and
   after immutable field-vector sharing, without changing the Fun program.

## Further Reading

- Standard derived instances and the Prelude [@haskell2010].
- Instance contexts and entailment [@jones1999].
- Classes and derived instances in the Haskell Report [@haskell2010].
- Operational semantics and evaluation strategies [@pierce2002].
