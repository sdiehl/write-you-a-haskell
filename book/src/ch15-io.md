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

Until now, the driver has evaluated `main` and printed its value. That is enough
for arithmetic and data structures, but gives a program no control over when to
print or read. Putting an effectful print function directly into the pure
language would make ordinary expression evaluation perform effects. Instead, we
need values that describe computations and an explicit boundary that runs them.

Chapter 14 already supplied the machinery for abstracting over a type
constructor. `Functor`, `Applicative`, and `Monad` use that machinery to express
mapping and sequencing. Their instances determine what sequencing means:
propagating `Nothing`, carrying an `Either` error, enumerating list results, or
performing IO. The syntax is shared. The dictionaries select the behavior.

## The language, now

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

The central bind operation has the qualified type below. The class parameter
`m` has kind `* -> *`. Its dictionary chooses the operation [@jones1993].

$$
\mathsf{Monad}\ m \Rightarrow m\ \alpha \to (\alpha \to m\ \beta) \to m\ \beta
$$

For IO, the source checker treats `IO` as an abstract nominal constructor. Core
represents an action as a function from unit to its result:

$$
\mathsf{IO}\ \alpha \quad\Rightarrow\quad () \to \alpha
$$

This is a representation translation, not an equality available to source
programs. A user cannot apply an action to `()` to run it. The trusted Prelude
implements `returnIO` by returning a unit-taking function, and `bindIO` by
returning a function that runs the first action and then its continuation.
Effectful primitives occur inside those function bodies.

Strictness still applies. Constructing `putStrLn "hello"` evaluates its string
argument and returns an action. It does not print. Constructing an action with
`error "bad"` as an eagerly demanded argument can fail before the action runs.
Sequencing controls effects, not evaluation of arbitrary pure arguments. The
Haskell report describes the analogous do translations [@haskell2010], but its
lazy evaluation must not be imported into Fun's operational account.

The monad laws describe intended instance behavior. The compiler checks types,
not those equations. In particular, equations involving nontermination need
care in a strict language. Dictionary passing remains the mechanism from
[Chapter 14](ch14-type-classes.md): no new class dispatch appears in the evaluator.

## Implementation

The parser calls `desugar::do_block` before lexical renaming. Processing the
statements backwards makes the remaining block the continuation of each bind.
A variable pattern becomes a lambda directly. A refutable pattern introduces a
case with a failure alternative. Its temporary name cannot be written as a
source identifier.

```rust
#![function!("../../crates/ch15_io/src/desugar.rs", do_block)]
```

This placement differs from a separate post-renaming desugaring pass: generated
bindings go through the same scope checker as handwritten ones. The helper
`do_pattern` validates the pattern-shaped syntax on the left of `<-`. The
span-bearing `DoFail` node is later translated into an ordinary error call with
its file, line, and column. No do node reaches Core.

Only the Prelude's trusted `FileId` lets the checker relate its IO implementation
to unit-taking functions. User-visible schemes remain nominal. After
elaboration, `io_lower::lower` traverses all Core types before lint: binder
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

The corpus migration is an observable interface change. Successful old programs
retain their expression in `_old`, then print it from IO `main`. Values that the
old diagnostic printer could display sometimes need an explicit `Show` instance
or a concrete type annotation. `BREAKING.md` lists those changes. `--print-value`
keeps the former runner available for compiler tests and fragment inspection.

## Running it

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

Use `--pass=core` to inspect dictionary arguments and the unit-taking lambda.
The 314-program corpus includes 30 new programs covering sequencing, lexical
scope, opaque IO, pattern failure, input, EOF, flushing, and exit status. At this
stage the Core interpreter remains the only execution engine.

## Exercises

1. **Warm-up:** replace the second `Just` in the example with a suitably annotated
   `Nothing`. Explain why its continuation is not called.
2. **Warm-up:** compare constructing an unused print action with constructing one
   whose string argument calls `error`.
3. **Intermediate:** trace the generated scopes for a do block containing a
   `let`, a tuple bind, and a shadowing bind.
4. **Intermediate:** inspect a `Monad IO` dictionary in Core and identify every
   type affected by IO lowering.
5. **Advanced:** extend the pipe-handshake test to issue two prompts and verify
   the order without relying on a terminal's line buffering.

## Further reading

- The report's do expressions and monadic library [@haskell2010].
- Higher-kinded constructor classes [@jones1993].
- Dictionary passing and qualified types [@jones1999].
