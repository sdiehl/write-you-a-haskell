# Chapter 7: Design of Fun

> **New files**
>
> - `src/session.rs`
> - `src/core.rs`
> - `src/typecheck.rs`
> - `src/elaborate.rs`
> - `src/diagnostics/codes.rs`
> - `src/diagnostics/mod.rs`
> - `src/parse.rs`
> - `src/source.rs`
>
> **Changed files**
>
> - `src/eval.rs`
> - `src/ast.rs`
> - `src/lexer.rs`
> - `src/lib.rs`
> - `src/main.rs`
> - `src/names.rs`
> - `src/types.rs`

## Motivation

The foundation chapters isolated particular ideas. We now bring their useful
parts into one compiler that will grow throughout the rest of the book. Each
later chapter owns a complete copy of this compiler. The preceding copy remains
runnable, so a reader can inspect a pass before and after its extension.

That arrangement needs more than a parser and evaluator. Every pass must agree
on source locations, names, diagnostics, and how failure stops compilation.
Tests must also survive the copying process. These interfaces are deliberately
small: a session owns the shared state, while a pass consumes one representation
and returns another. The pipeline figure in Chapter 1 shows where later
representations will fit.

## The language, now

A program is an explicit brace block containing declarations. Semicolons separate
declarations. A trailing semicolon is optional. Each declaration may refer to
itself and to earlier declarations. Forward references and mutual recursion wait
until Chapter 9.

```fun
{
  twice f x = f (f x);
  sum n = if n == 0 then 0 else n + sum (n - 1);
  main = twice (\x -> x + 1) (sum 10);
}
```

Local `let` uses braces too, with one binding in this snapshot. Recursive bindings
need no `rec` keyword: a self-reference identifies recursion, and the right-hand
side must be a lambda. `let { x = x; } in x` is rejected with `E0120`.

`Char` contains a Unicode scalar value, and `String` is a primitive UTF-8 string.
Strings concatenate with the monomorphic `++` operator. Integers accept hexadecimal
notation and underscores. Both line comments and nested block comments are
supported.

```fun
{ {- outer {- nested -} -} main = "Hello, " ++ "λ\n"; }
```

`run` still prints the value of `main`, including quotes and escapes for strings.
It does not yet execute `IO`. Integer `div` and `mod` are curried built-in
functions with floor semantics: `div 7 (-3)` is `-3`, and `mod 7 (-3)` is `-2`.
The old calculator's truncating division fixture changes accordingly.

## Theory

A source program and an executable intermediate language serve different
purposes. The source tree keeps spelling, syntax, and source spans. Core keeps
only the operations needed for evaluation. Elaborating one into the other makes
language conveniences explicit. Later we add types to Core so that a separate
checker can validate each transformation [@pierce2002].

Strictness remains a semantic requirement. The function is evaluated before its
argument, operator operands are evaluated left to right, and a conditional
selects only one branch. Top-level values are initialized once, in declaration
order in this snapshot, before the final value of `main` is selected. Even an
unused failing initializer stops the program. Top-level function bodies wait
until application.

The current type system remains Hindley–Milner, using the level-based checker
from Chapter 6 [@damas1982; @kiselyov2013]. The recursive-binding restriction
avoids constructing a value before its own initialization has completed.
Recursive functions instead refer to their own closure when entered. Ordinary
name scopes still use spelling-based maps here. Unique binding identities become
operational in Chapter 9.

## Implementation

### Source and session state

`source.rs` gives a file its own identity and stores a line-start table. Spans
are half-open byte ranges, matching the lexer, while displayed columns count
Unicode scalar values. The file identity prevents equal offsets in different
files from being confused.

```rust
{{#include ../../crates/ch07_fun/src/source.rs:source}}
```

`names.rs` supplies a handwritten interner and fresh-name supply. Repeated
spellings share a `Symbol`. Repeated calls to the supply produce distinct
`Name` identities. These definitions establish the interface for the later
renamer without changing lexical scoping prematurely.

A session owns these facilities along with options and a diagnostic sink:

```rust
{{#include ../../crates/ch07_fun/src/session.rs:session}}
```

Each pass takes `&mut Session` and returns `Result<Output, Stop>`. A `Stop` carries
no second copy of the diagnostics. The driver renders the accumulated structured
errors and exits before calling the next pass. Lexing and duplicate-definition
checks collect errors. Inference stops where a type error prevents a meaningful
result.

`diagnostics/codes.rs` is the stable registry for errors and warnings, including
codes reserved for upcoming features. A diagnostic has a severity, message,
source labels, and notes. Ariadne renders it. The first two stderr lines remain
the stable code and primary source location used by the corpus runner.

### Surface checking and Core

`lexer.rs` uses logos, including callbacks for nested comments and literal
escapes. Its reserved-word and operator tables also generate the HTML and PDF
syntax definitions through `cargo xtask syntax`. The book build checks these
outputs for drift.

`parser.lalrpop` builds spanned expressions and top-level bindings. Equation
parameters become nested lambdas, and duplicate parameters are rejected. The
checker processes declarations in source order. For a self-dependent binding,
it introduces a provisional monotype while checking the lambda, unifies the
result, then generalizes as in the previous chapter.

`elaborate.rs` lowers the checked tree into a separate Core representation:

```rust
{{#include ../../crates/ch07_fun/src/core.rs:core}}
```

Operators become saturated primitive calls. Negation becomes subtraction from
zero. The initial built-in environment provides curried wrappers for `div` and
`mod`. `--no-prelude` removes those wrappers for isolated tests. The source
Prelude arrives with datatypes. Source names beginning with `prim_` are rejected
with `E0111` outside that future Prelude path.

`--lint` currently checks Core scope, primitive saturation, and the recursive
lambda invariant. It is structural lint, not a type checker. Chapter 10 will
replace it with the full System F check. The golden harness enables it on every
run.

### Reference evaluation

`eval.rs` interprets Core directly. A recursive closure stores its self name
without creating a cyclic Rust environment. Application inserts that closure
into its captured environment before the parameter:

```rust
{{#include ../../crates/ch07_fun/src/eval.rs:application}}
```

The program runner initializes every top-level binding in order and retains
those values in its environment. Integers normalize to 63 bits after arithmetic,
and division checks zero before computing its floor quotient and matching
remainder. Character and string printing share the source literal escape rules.

This reference evaluator is a recursive tree walker on a 512 MiB thread stack.
It has a fuel limit for experiments, but no constant-stack guarantee. The explicit
machine from Chapter 5 returns as an ANF oracle in Chapter 18. Keeping Core's
reference interpreter direct makes the forthcoming elaboration passes easier
to inspect.

## Running it

```text
$ cd crates/ch07_fun
$ cargo run -q -- run tests/programs/fun-top.fun
42
$ cargo run -q -- dump --pass=types tests/programs/fun-string.fun
main : String
$ cargo run -q -- dump --pass=core --no-prelude tests/programs/fun-char.fun
val main = 'λ'
$ cargo run -q -- repl
Fun: expressions, :type EXPR, :core EXPR, :quit.
fun> :type \x -> x
main : a -> a
fun> :quit
```

Available dumps are `tokens`, `parse`, `types`, and `core`, through either
`dump --pass=...` or the common `--dump=...` option. The initial REPL evaluates
individual expressions. Persistent JIT definitions arrive later. All 65 inherited
programs are migrated explicitly, and 15 new programs bring the corpus to 80.
Ten Core snapshots expose representative lowering results.

## Exercises

1. **Easy.** Use `--dump=tokens` to compare a nested comment with the operator `-->`.
2. **Medium.** Add an unused top-level initializer that divides by zero. Explain why `main` never produces a value.
3. **Medium.** Inspect Core for a local recursive function and explain where the self reference becomes bound.
4. **Hard.** Construct an invalid Core program that passes source parsing but could only be produced by a faulty compiler transformation. Extend a lint test to reject it.

## Further reading

The Haskell Report provides the lexical and scoping reference that Fun gradually
adopts [@haskell2010]. Pierce develops the typed intermediate-language perspective
[@pierce2002].
