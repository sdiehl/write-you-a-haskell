# Chapter 2: Parsing

> **New files**
>
> - `src/lexer.rs`
> - `src/pratt.rs`
> - `src/ast.rs`
> - `src/eval.rs`
> - `build.rs`
> - `src/diagnostic.rs`
> - `src/lib.rs`
> - `src/main.rs`
>
> **Changed files**
>
> None.

## Motivation

A parser turns a sequence of tokens into a tree. The tree must make choices
that the text leaves implicit: multiplication binds more tightly than addition,
and repeated subtraction groups to the left. If those choices are wrong,
a correct evaluator still computes the wrong answer.

We implement the same language twice. LALRPOP generates a parser from a grammar.
A small Pratt parser expresses precedence directly as binding powers. Their
agreement is useful evidence, but we also test known answers: two implementations
can agree on the same mistake. Unlike the full language, this calculator has no
functions, types, comments, or top-level declarations.

## The language, now

A program is one expression. It has decimal integer literals, names, `+`, `-`,
`*`, `/`, unary minus, parentheses, and a non-recursive `let`.

```fun
let x = 7 in (let y = x + 1 in y * y) - x
```

The binding is visible in its body, not its right-hand side. A nested binding
shadows an outer name. Multiplication and division bind above addition and
subtraction. All four operators associate left. Unary minus binds above them.
The `/` operator truncates integer division toward zero in this mini-language.
Full Fun will reserve `/` for fractional arithmetic and spell this operation
`quot`.

## Theory

A grammar with a single production for every binary operator leaves multiple
possible trees for `1 + 2 * 3`. Splitting expressions into sums, products, and
atoms encodes precedence structurally. Left recursion gives left associativity.
Parentheses re-enter the complete expression grammar.

### Choosing a parsing strategy

Parsing techniques answer different questions. A grammar describes the accepted
syntax. A parsing algorithm recognizes it. An API determines how we express the
parser in code. Keeping those distinctions clear prevents misleading comparisons.

| Approach           | Main idea                                                                                | Tradeoff for this book                                                                       |
| ------------------ | ---------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| LR(1) / LALR(1)    | A generated state machine shifts tokens and reduces grammar productions.                 | A declarative whole-language grammar, with conflicts reported during generation.             |
| Pratt              | An expression parser uses binding powers to decide which operator owns the next operand. | Small, readable operator parsing. Declarations and blocks still need their own grammar.      |
| Packrat            | Memoize parsing results by rule and input position.                                      | Predictable recognition time for a fixed suitable grammar, at the cost of a memo table.      |
| Parser combinators | Compose small parsers using ordinary functions.                                          | Convenient abstraction. Backtracking, memoization, and error behavior depend on the library. |

**LR(1)** scans left to right and constructs a rightmost derivation in reverse.
Its state summarizes the prefix already read. One lookahead token helps decide
whether to shift another token or reduce a completed production. The “1” does
not limit expressions to one token. The parser's stack retains arbitrarily deep
syntactic context. Left recursion, such as a sum containing a smaller sum, fits
this bottom-up process naturally.

**LALR(1)** merges LR(1) states with the same underlying LR(0) items, combining
their lookahead sets. This reduces table size but can introduce conflicts absent
from the unmerged grammar. Despite its name, our generator uses LR(1) by default.
LALR(1) is an option. See the [LALRPOP documentation](https://github.com/lalrpop/lalrpop#lalrpop).
A conflict is useful feedback: the grammar needs attention before we can trust
its generated parser.

**Pratt parsing** makes the operator decision local. After reading `1 + 2`, the
parser compares the following `*` with the binding power of `+`. Multiplication
belongs inside the right operand, so `1 + 2 * 3` becomes `1 + (2 * 3)`. For
`8 - 3 - 1`, the right-operand threshold excludes an equal-strength subtraction,
giving `(8 - 3) - 1`. Prefix and infix uses of `-` can have different handlers.
This is an excellent fit for expressions, but does not by itself specify the
structure of a class declaration or a layout block.

**Packrat parsing** avoids repeated work during backtracking by remembering a
rule's success or failure at each position. It is commonly associated with
parsing expression grammars (PEGs), whose alternatives use ordered choice: the
first successful alternative wins. Reordering alternatives can therefore change
the language. For a fixed grammar with bounded work per memoized entry, this
gives linear recognition time and generally linear memo storage. Ordinary
left-recursive rules need rewriting or a specialized extension. Ford's
[packrat and PEG overview](https://bford.info/packrat/) develops these choices.

**Parser combinators** are a way to write parsers, not one recognition algorithm.
A sequencing combinator runs one parser after another. A choice combinator tries
alternatives according to its library's rules. A combinator library may commit,
backtrack, or memoize. Its pleasant surface does not automatically give linear
time, good recovery, or support for left recursion. Those remain design choices.

### Why LALRPOP and a comparison Pratt parser?

LALRPOP gives us one inspectable grammar for the language as a whole, typed Rust
actions for constructing the AST, and explicit recovery productions. Pratt gives
us a compact second account of the calculator's precedence rules. Implementing
both here makes the grouping decisions concrete and gives us an independent
comparison. The later compiler keeps LALRPOP. It does not run both parsers.

Haskell-style syntax also needs work around the grammar. In Chapter 8, logos
recognizes tokens, marginalia retains comments and whitespace for formatting,
and offsides translates indentation into block delimiters. The latter two
libraries supply the trivia and Haskell-style layout support this frontend
needs. LALRPOP then consumes that prepared token stream. Layout remains visible
as a separate, testable transformation instead of being scattered through parser
actions. This calculator has neither layout nor comment preservation yet.

User-declared fixities introduce a separate issue. A generated parser cannot
have a different precedence table for each source file. Chapter 8 parses flat
operator chains and resolves their grouping afterward. Neither a Pratt loop nor
a parser generator removes the need to decide when those declarations become
known.

### Syntax and typing

A typing judgment already describes this small language. If two operands have
type `Int`, their sum has type `Int`:

$$
\frac{\Gamma \vdash e_1 : \mathsf{Int} \quad \Gamma \vdash e_2 : \mathsf{Int}}
{\Gamma \vdash e_1 + e_2 : \mathsf{Int}}\;\text{(Add)}
$$

This chapter does not implement an inferencer: integers are the only values.
It does check lexical scope before evaluating. Pierce develops the distinction
between syntax, typing, and evaluation [@pierce2002]. The Haskell Report is useful
for comparison, although Fun's later fixity rules are deliberately absent here
[@haskell2010].

## Implementation

`lexer.rs` uses logos to recognize tokens and retain byte spans. Keywords win over
identifiers of the same length. An invalid character or an out-of-range literal
produces `E0001`. `ast.rs` stores a span on each expression, making both parsers
comparable without losing source information.

```rust
{{#include ../../crates/ch02_calc/src/ast.rs:ast}}
```

The generated parser implements precedence with nonterminals. The complete
grammar also handles bindings, parentheses, unary minus, and a recovery production.

```text
{{#include ../../crates/ch02_calc/src/parser.lalrpop:precedence}}
```

An `!` production inserts an error node and stores the recovery diagnostic. The
driver returns errors even when recovery produces a tree. The evaluator never
receives recovery nodes. `diagnostic.rs` prints a stable error-code line and
location, then lets ariadne render the source excerpt with byte indexing.

```rust
{{#include ../../crates/ch02_calc/src/lib.rs:parse}}
```

The Pratt parser's loop consumes operators whose binding power is high enough.
Parsing the right operand at `power + 1` makes the operator left-associative.
Prefix parsing handles names, literals, parentheses, and negation. A separate
entry handles `let`.

```rust
{{#include ../../crates/ch02_calc/src/pratt.rs:pratt}}
```

`eval.rs` uses a stack of bindings, searched from the end, to model lexical scope.
It evaluates a binary operator's left operand before its right operand and
restores the environment even if a binding body fails. Arithmetic uses wrapping
63-bit values from the start, avoiding dependence on Rust's debug overflow checks.
The normalization function is included as a Rust item:

```rust
#![function!("../../crates/ch02_calc/src/eval.rs", normalize)]
```

```rust
{{#include ../../crates/ch02_calc/src/eval.rs:evaluate}}
```

`main.rs` connects parsing, scope checking, execution, dumps, and the rustyline
REPL. The corpus tests the command-line boundary. Separate tests compare complete
ASTs from 200 deterministic generated expressions and snapshot ten parse dumps.

## Running it

```text
$ cd crates/ch02_calc
$ cargo run -q -- run tests/programs/precedence.fun
7
$ cargo run -q -- dump --pass=parse tests/programs/precedence.fun
(+ 1 (* 2 3))
$ cargo run -q -- run tests/programs/precedence.fun --parser=pratt
7
$ cargo run -q -- repl
Calc: :quit to exit, :parse EXPR to inspect the AST.
calc> let x = 6 in x * 7
42
calc> :quit
```

`check` verifies syntax and names without executing arithmetic. Thus `1 / 0`
passes `check` but `run` reports `fun: divide by zero` and exits with status 1.
Use `dump --pass=tokens` to inspect lexer spans.

## Exercises

1. **Easy.** Predict both the value and the parse dump of `20 - 3 * 4 - 1`, then run it.
2. **Easy.** Compare the spans of `1` and `(1)`. Explain why their values agree.
3. **Medium.** Change the Pratt parser's right binding power to `power`. Find the smallest subtraction expression that exposes the associativity change.
4. **Medium.** Write a malformed expression that reports two independent syntax errors. Explain where recovery can resume.
5. **Hard.** Extend the generator to insert redundant parentheses and verify that evaluation remains unchanged.

## Further reading

Pierce separates syntax and semantics carefully [@pierce2002]. The Haskell
Report specifies a much larger concrete syntax and fixity-resolution procedure
[@haskell2010].
