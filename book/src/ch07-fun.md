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

We've built a parser, explored how functions run, and taught a compiler to
infer types. Now we can bring those pieces together. From this chapter onward
we'll be building Fun, the language we'll eventually compile to native code.
Each chapter keeps a runnable snapshot, so you can always return to a simpler
version when you want to see where an idea began.

Some of the work here is the ordinary engineering that makes the interesting
parts pleasant to build. When the type checker finds a mistake, it needs to
point back to the source. When one pass fails, the next shouldn't try to make
sense of its unfinished output. We'll give names, source locations, and errors
a shared home, then connect our passes into a compiler we can keep extending.

## The Language, Now

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
need no `rec` keyword. A self-reference identifies recursion, and the right-hand
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
functions with floor semantics. For example, `div 7 (-3)` is `-3`, and `mod 7 (-3)` is `-2`.
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

## Running It

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

Try the `tokens`, `parse`, `types`, and `core` dumps on the same small program.
Each shows a different stage of the journey. You can use `dump --pass=...` or
the common `--dump=...` option. The REPL evaluates individual expressions for
now. Persistent definitions will arrive with the JIT.

## Exercises

1. **Easy.** Use `--dump=tokens` to compare a nested comment with the operator `-->`.
2. **Medium.** Add an unused top-level initializer that divides by zero. Explain why `main` never produces a value.
3. **Medium.** Inspect Core for a local recursive function and explain where the self reference becomes bound.
4. **Hard.** Construct an invalid Core program that passes source parsing but could only be produced by a faulty compiler transformation. Extend a lint test to reject it.

## Further Reading

The Haskell Report provides the lexical and scoping reference that Fun gradually
adopts [@haskell2010]. Pierce develops the typed intermediate-language perspective
[@pierce2002].
