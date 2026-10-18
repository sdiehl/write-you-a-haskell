# Chapter 15: Monads, Do Notation and IO

> **New files**
>
> - `src/io_lower.rs`
>
> **Changed files**
>
> - `src/desugar.rs`
> - `src/eval.rs`
> - `src/typecheck.rs`
> - `src/ast.rs`
> - `src/data.rs`
> - `src/elaborate.rs`
> - `src/fixity.rs`
> - `src/kinds.rs`
> - `src/layout.rs`
> - `src/lexer.rs`
> - `src/lib.rs`
> - `src/main.rs`
> - `src/primitives.rs`
> - `src/rename.rs`
> - `src/session.rs`

## Motivation

So far, our programs have had a rather limited relationship with the outside
world. The driver evaluates `main`, prints the answer, and goes home. Let's
write programs that can ask a question, read a response, and decide what to do
next.

We want to keep ordinary expressions pure, so evaluating an IO value won't
perform its effects. We'll represent an action as a value and run it at an
explicit boundary. That distinction lets us build and combine actions using
the functions we already know.

The type classes from Chapter 14 give us a way to describe these combinations.
`Functor`, `Applicative`, and `Monad` work with IO, but they also describe useful
operations on `Maybe`, `Either`, and lists. We'll start with what the operations
do and expand the syntax by hand. There's plenty to understand here without
adding mystique to the vocabulary.

## The Language, Now

A complete program now has `main :: IO ()`, whether that signature is written or
inferred. The smallest example calls the new Prelude printer:

```fun
{{#include ../../crates/ch15_io/tests/programs/io/hello.fun}}
```

`do` notation also works for ordinary data. Here the two binds extract values
from `Just`, and `pure` constructs the result in the same `Maybe` instance:

```fun
{{#include ../../crates/ch15_io/tests/programs/io/maybe.fun}}
```

A block must end with an expression. A statement `x <- action` binds `x` only in
the remaining statements. A statement `let` introduces local declarations
without an `in`. Patterns may be nested, including tuples, lists, as-patterns,
and nominal record patterns. A failed refutable pattern calls `error` with its
source location. Fun has no `MonadFail`, so failure is a runtime error even when
the surrounding monad is `Maybe`.

`putStr`, `putStrLn`, `getLine`, `print`, and `exitWith` provide the program's
external interface. `mapM`, `mapM_`, `sequence_`, `when`, and `unless` express
common sequencing patterns. A missing or wrongly typed entry point reports
`E0250`. A body that violates its explicit signature can instead fail during
signature checking with `E0203`.

## Theory

Suppose an action produces an `a`, and the rest of our program knows how to
use that `a` to make the next action. Bind connects the two. Its second argument
is a function because the next action may depend on the first result. Read
this type slowly from left to right [@jones1993].

$$
\mathsf{Monad}\ m \Rightarrow m\ \alpha \to (\alpha \to m\ \beta) \to m\ \beta
$$

Let's expand a small program before worrying about its representation.

```fun
main = do
  name <- getLine
  putStrLn name
```

The sequencing becomes `getLine >>= (\name -> putStrLn name)`. The lambda
is essential. We cannot construct the second action using `name` until the
first action supplies it. Building this combined action reads nothing and
prints nothing. When the driver runs it, we read a line, pass the result to
the lambda, and run the action it returns.

For comparison, `let unused = putStrLn "hello" in pure ()` constructs a
printing action and discards it. Running the resulting action prints nothing.
We still evaluate pure arguments strictly when constructing an action.

For IO, the source checker treats `IO` as an abstract nominal constructor. Core
represents an action as a function from unit to its result:

$$
\mathsf{IO}\ \alpha \quad\Rightarrow\quad () \to \alpha
$$

The unit argument gives us somewhere to put the delay. Constructing the
function doesn't run its body. `returnIO` builds a function that returns its
saved value, while `bindIO` builds one that runs the first action and passes
the result to the next. The effectful primitives sit inside those bodies.

Only the trusted Prelude and compiler can use this representation. Source
programs see abstract `IO a` values, so they can't apply an action to `()`
and run it wherever they please. That boundary is what lets ordinary Fun
expressions remain pure.

Strictness still applies. Constructing `putStrLn "hello"` evaluates its string
argument and returns an action. It does not print. Constructing an action with
`error "bad"` as an eagerly demanded argument can fail before the action runs.
Sequencing controls effects, not evaluation of arbitrary pure arguments. The
Haskell report describes the analogous do translations [@haskell2010], but its
lazy evaluation must not be imported into Fun's operational account.

The monad laws describe intended instance behavior. The compiler checks types,
not those equations. In particular, equations involving nontermination need
care in a strict language. Dictionary passing remains the mechanism from
[Chapter 14](ch14-type-classes.md). No new class dispatch appears in the evaluator.

## Implementation

The parser calls `desugar::do_block` before lexical renaming. Processing the
statements backwards makes the remaining block the continuation of each bind.
A variable pattern becomes a lambda directly. A refutable pattern introduces a
case with a failure alternative. Its temporary name cannot be written as a
source identifier.

```rust
#![function!("../../crates/ch15_io/src/desugar.rs", do_block)]
```

This placement differs from a separate post-renaming desugaring pass. The generated
bindings go through the same scope checker as handwritten ones. The helper
`do_pattern` validates the pattern-shaped syntax on the left of `<-`. The
span-bearing `DoFail` node is later translated into an ordinary error call with
its file, line, and column. No do node reaches Core.

Only the Prelude's trusted `FileId` lets the checker relate its IO implementation
to unit-taking functions. User-visible schemes remain nominal. After
elaboration, `io_lower::lower` traverses all Core types before lint, including binder
types, type applications, constructor fields, match result types, and dictionary
types. Replacing only `IO a` at the outermost level would miss higher-kinded
uses such as a `Monad IO` dictionary. The traversal also replaces the unapplied
constructor `IO` with the partially applied function constructor `(->) ()`.

The runner first installs function closures and evaluates strict top-level
values in dependency order, then applies the entry point to unit:

```rust
#![function!("../../crates/ch15_io/src/eval.rs", run_io)]
```

An unused top-level action is still a value initialized at startup. Its body is
not entered, as this regression demonstrates:

```fun
{{#include ../../crates/ch15_io/tests/programs/io/deferred.fun}}
```

The evaluator distinguishes a language error from a requested process exit.
This distinction prevents `exitWith 7` from being printed as an error message:

```rust
#![enum!("../../crates/ch15_io/src/eval.rs", Failure)]
```

The driver flushes stdout at completion and before runtime diagnostics. Input
primitives flush before reading, so a prompt reaches the user before the program
blocks. The test runner accepts `.in` files and sends identical input to each
engine. A pipe-handshake regression checks flushing independently of terminal
buffering.

Existing programs need a small adjustment here. The driver no longer prints
`main` for us, so we put the printing in the program itself. If a value has no
`Show` instance, we'll need to supply one or choose something printable from it.
The old value printer remains available through `--print-value` for inspecting
compiler fragments.

## Running It

From the repository root, enter the chapter crate. These commands print an
explicit action's output and its inferred entry-point type:

```text
$ cd crates/ch15_io
$ cargo run -q -- run tests/programs/io/hello.fun --lint
hello
$ cargo run -q -- dump --pass=types tests/programs/io/hello.fun
main : IO ()
$ cargo run -q -- run tests/programs/io/deferred.fun --lint
ok
```

Use `--pass=core` to find the dictionary arguments and the lambda taking unit.
It's worth comparing an unused print action with one that is actually sequenced
by `main`. Both are values, but only the latter's body runs. The Core interpreter
is still our execution engine in this chapter.

## Exercises

1. **Warm-up.** Replace the second `Just` in the example with a suitably annotated
   `Nothing`. Explain why its continuation is not called.
2. **Warm-up.** Compare constructing an unused print action with constructing one
   whose string argument calls `error`.
3. **Intermediate.** Trace the generated scopes for a do block containing a
   `let`, a tuple bind, and a shadowing bind.
4. **Intermediate.** Inspect a `Monad IO` dictionary in Core and identify every
   type affected by IO lowering.
5. **Advanced.** Extend the pipe-handshake test to issue two prompts and verify
   the order without relying on a terminal's line buffering.

## Further Reading

- The report's do expressions and monadic library [@haskell2010].
- Higher-kinded constructor classes [@jones1993].
- Dictionary passing and qualified types [@jones1999].
