# Chapter 16: Pattern-Match Compilation

> **New files**
>
> - `src/match_compile.rs`
> - `src/usefulness.rs`
> - `src/env.rs`
>
> **Changed files**
>
> - `src/core.rs`
> - `src/core_lint.rs`
> - `src/eval.rs`
> - `src/io_lower.rs`
> - `src/lib.rs`
> - `src/main.rs`

## Motivation

Our interpreter tries patterns from top to bottom, which makes their meaning
easy to follow. A native compiler needs to turn that process into instructions
that inspect tags, load fields, and branch. We could translate each pattern
separately, but we'd often inspect the same value again and again.

Instead, we'll look at the patterns together. Put them in a matrix, choose a
useful test, and divide the remaining work according to its result. This gives
us a decision tree. Join points let different paths share the code they reach,
including the next clause when a guard fails.

We can also ask whether any inputs slip through the patterns, or whether an
earlier clause has already caught everything a later clause could match. It's
a pleasing connection. Understanding the shapes of values helps us both run
patterns and explain mistakes in them.

## The Language, Now

There is no new source syntax. Earlier programs keep the same results, including
guards that fail after their patterns match:

```fun
{{#include ../../crates/ch16_match/tests/programs/match/guard-fallback.fun}}
```

The first clause recognizes `Just 42`, but its guard fails. Execution must try
the second clause and return `42`. It must not report pattern failure or skip
to the `Nothing` clause. Local `where` bindings remain in scope for the guards
and right-hand side of their own equation.

The checker can now show us holes in a match. `W0001` includes example inputs
that aren't covered, and `W0002` points out a clause that earlier clauses have
made unreachable. These are warnings, so the program can still run. Encountering
an uncovered input at runtime still reports
`fun: pattern match failure at FILE:LINE:COL`.

## Theory

Think of a match as a matrix whose rows are clauses and whose columns are
scrutinees. Specializing a column for constructor `C` keeps rows that can match
`C`, removes that constructor, and inserts its fields as new columns. A default
matrix retains rows whose selected pattern can match any other value. This is
the decision-tree approach described by Maranget [@maranget2008].

Fun chooses the first column containing a refutable pattern in the first row.
That simple heuristic is deterministic and easy to trace. It is not a claim to
minimize the number of tests for every matrix. Wildcards, variables, and
as-patterns do not inspect a value. Their bindings are recorded for the eventual
clause body. Constructor patterns expose fields only after their tag has matched.

Finite datatypes have known constructor families. Integer and character literals
need a default path because a finite set of tested literals cannot exhaust their
value domain. String patterns become equality tests. Double patterns also use
primitive equality, preserving the language's floating comparison behavior,
including equality of positive and negative zero.

Usefulness asks whether a candidate pattern vector admits a value not matched
by preceding covering rows [@maranget2007]. A useless source row is redundant.
A useful all-wildcard candidate supplies a missing-case witness. Guards complicate
coverage because a matching pattern may still fall through. The implementation treats
a guarded row as covering only when one guard is literally `True`. It does not
attempt to prove arbitrary Boolean expressions.

### One Question, Two Checks

There's a neat way to turn both warnings into the same question. Does this
pattern match anything the earlier patterns missed? Maranget calls such a
pattern _useful_. We'll write `U(P, q)` for this question, with earlier rows
`P` and candidate pattern vector `q`. A witness is an actual shape that slips
past `P` and matches `q` [@maranget2007].

For reachability, `P` contains the covering rows before the current clause and
`q` is that clause's pattern vector. For exhaustiveness, `P` contains all
covering rows and `q` is an all-wildcard vector. The first query asks whether a
clause adds any cases. The second asks whether anything is still missing.
It's the rows together that determine coverage. A clause can be unreachable
even when no single earlier clause covers it completely.

Consider a function with two arguments, `Maybe Bool` and `Bool`:

```fun
choose Nothing     _     = 0
choose (Just True) _     = 1
choose (Just _)    False = 2
```

Ignore the right-hand sides and retain the argument columns:

| Row | First Argument | Second Argument |
| :-- | :------------- | :-------------- |
| 1   | `Nothing`      | `_`             |
| 2   | `Just True`    | `_`             |
| 3   | `Just _`       | `False`         |

Ask whether `(_, _)` is useful. The first column contains both constructors of
`Maybe`, so examine each constructor family member. Specializing for `Nothing`
discards rows 2 and 3 and leaves a wildcard for the second argument. That branch
is covered. Specializing for `Just` discards row 1 and replaces the first column
with the constructor's single field:

| Row | Value Inside `Just` | Second Argument |
| :-- | :------------------ | :-------------- |
| 2   | `True`              | `_`             |
| 3   | `_`                 | `False`         |

The `True` branch is covered by row 2. In the `False` branch, only row 3 remains,
and it covers a second argument of `False`. The uncovered second argument is
`True`. Rebuilding the constructor around the field gives the witness
`(Just False, True)` for the original argument vector. Adding
`choose (Just False) True = 3` completes this match. Any subsequent clause is
unreachable, even though no original clause alone covered every input.

### Specialization and the Default Matrix

`usefulness.rs` implements the search on the first column. The selected pattern
and constructor family determine the next problem:

1. **Constructor candidate.** For `C(p1, ..., pk)`, keep rows headed by `C` or a
   wildcard. Replace `C` by its fields. Expand a wildcard to `k` wildcards. Drop
   rows headed by other constructors, and make the same expansion in the query.
2. **Wildcard candidate, complete family.** If every constructor of the column's
   datatype appears, specialize for each. Any useful branch supplies a witness.
3. **Wildcard candidate, incomplete family.** Search the default matrix: retain
   only wildcard-headed rows and drop their first column. A useful suffix can
   be prefixed by a missing constructor. Literal domains are handled through
   this path, choosing a literal absent from the tested heads.

The zero-column case distinguishes an empty matrix from an empty row. With no
rows, nothing covers the remaining empty vector, so it is useful. With an empty
row, an earlier clause already matches everything left, so it is not useful.
These small base cases let the search handle nested constructors without
enumerating all values of a recursive datatype.

On return, the algorithm rebuilds each stripped constructor around the witness
fields. Fun's `useful` therefore returns an optional pattern vector, rather than
just a Boolean. The warning driver can print a concrete missing shape, add that
shape to its temporary coverage matrix, and search again for up to three examples.

### Guards and the Limits of Coverage

A guarded clause can be unreachable because earlier clauses already cover its
patterns. It can't usually establish coverage itself, since its guard might fail.
Fun checks the row against earlier coverage, then adds it only if it is unguarded
or has a syntactically literal `True` guard. It does not solve arbitrary guard
expressions. Consequently a reported gap can reflect this conservative policy,
and a useful pattern does not prove that its guard will ever succeed.

The analysis uses typed constructor families and treats variables as wildcards.
As-pattern names do not change coverage. It does not prove that recursive field
types are inhabited. Compilation and checking remain separate. The decision tree
must preserve clause order and guard fallthrough regardless of these warnings.

### Sharing the Decision Tree

A join point names a continuation local to an expression. Its parameters carry
the values needed by the shared body, and every jump is a fully applied tail
transfer. Unlike an ordinary function, a join cannot escape or be passed as a
value. This distinction anticipates the later use of joins in ANF
[@maurer2017].

## Implementation

The compiler's internal row separates the remaining patterns from the eventual
right-hand side. `values` records the values bound along the current decision
path. `target` and `params` identify the shared clause body.

```rust
#![struct!("../../crates/ch16_match/src/match_compile.rs", Row)]
```

Before building tests, `Compiler::compile` assigns each clause a parameterized
right-hand-side join. It also compiles the residual matrix following that clause
into a failure join. Guards run in order. If all fail, a jump enters that residual
matrix. Several leaves can therefore reach one clause body without copying it,
and several failed guards can use one fallback.

A constructor branch must bind fields at the scrutinee's actual type arguments.
The helper looks up the declaration and substitutes its parameters:

```rust
#![function!("../../crates/ch16_match/src/match_compile.rs", Compiler::fields)]
```

For example, specializing `Just` at `Maybe Int` binds an `Int`, not the declaration's
uninstantiated type variable. The same rule matters for a record projection:
its single-constructor match now becomes a tag branch and ordinary field binders.
Core lint independently checks these instantiated types.

`Compiler::tree` handles the matrix recursively. An empty matrix becomes
`PatternFail`. A first row with no refutable patterns jumps to its clause join,
passing the values accumulated for its binders. Otherwise the selected column
produces a flat `Case`, or equality tests for string and Double literals, with
specialized subtrees and a default subtree.

The warning pass uses the original nested patterns, before that representation
is lost. Its specialization helper is the matrix operation traced above:

```rust
#![function!("../../crates/ch16_match/src/usefulness.rs", specialize)]
```

`useful` carries the candidate vector and column types alongside the matrix.
Its driver repeatedly asks for useful witnesses and records diagnostics:

```rust
#![function!("../../crates/ch16_match/src/usefulness.rs", warn)]
```

Generated do-pattern fallback rows share the originating span. The diagnostic
pass suppresses redundant warnings for those synthetic adjacent rows, while
retaining warnings for source clauses. Warning analysis does not choose the
runtime branches. A mistake in a warning must not silently change evaluation.

Core lint now checks flat alternative shape, constructor field types, globally
unique binders, and join argument types and arities. A jump must be in tail
position relative to its join. Entering a non-tail subexpression clears the
available outer joins. The subexpression may establish its own local joins.
The Core interpreter keeps evaluation and return frames in an explicit vector.
A source call changes the current expression and environment rather than calling
Rust recursively. A jump unwinds frames to its lexical join and transfers control
to that body. Closures borrow code through stable indices, so calling a function
does not clone its syntax tree. Constructor and environment destruction also use
a worklist, preventing a long list from overflowing the stack when released.
Tests run non-tail recursion and list reclamation on a 256 KiB host stack.

Saved environments share an immutable balanced tree. Capturing a scope copies
one `Rc`, while adding a binding copies only the search path. `env.rs` implements
the update with a work vector. This keeps a growing scope from being copied in
full whenever the interpreter saves a continuation. Recursive closures refer to
a machine-owned group by index, so calls reuse the group without creating a
reference-count cycle.

## Running It

```text
$ cd crates/ch16_match
$ cargo run -q -- run tests/programs/match/guard-fallback.fun --lint
42
$ cargo run -q -- dump --pass=types tests/programs/match/guard-fallback.fun
main : IO ()
```

For the transformation itself, dump the same file with `--pass=core` and then
`--pass=match`. The former retains nested `Match`. The latter exposes flat
`Case`, right-hand-side joins, and guard-failure jumps. The dumps include the
Prelude, so the small no-Prelude cases in `tests/matches.rs` are useful for
studying one decision tree at a time.

We check the translation by running matches both before and after compilation.
Coverage needs its own check. For small Boolean patterns we can enumerate all
possible inputs and compare that direct answer with the usefulness algorithm.
This catches a different kind of mistake from a wrong runtime branch.

## Exercises

1. **Warm-up.** Draw the decision tree for the example and mark the failing guard's
   destination.
2. **Intermediate.** Add a wildcard clause before a constructor clause and predict
   the redundant-clause warning before running the checker.
3. **Intermediate.** Specialize a two-column matrix whose first row begins with a
   variable and whose second pattern is a constructor.
4. **Advanced.** Change the column heuristic in an experimental copy. Compare
   tree sizes while keeping the full before/after execution corpus unchanged.
5. **Advanced.** Construct malformed Core that jumps to an outer join from a
   non-tail expression, and explain the continuation that would be discarded.

## Further Reading

- Decision trees and column-selection heuristics [@maranget2008].
- Usefulness, exhaustiveness, and redundancy [@maranget2007].
- Join points as a compiler intermediate representation [@maurer2017].
- Source pattern and guard semantics [@haskell2010].

For a standalone Rust treatment of the same matrix operations, see
[Patternkit](https://github.com/sdiehl/patternkit). Fun keeps its implementation
in the chapter so that the checking algorithm remains visible here.
