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

We read `1 + 2 * 3` and know that the multiplication comes first. The computer
gets a string of characters. Before it can calculate anything, we have to teach
it how those characters fit together. That's the parser's job.

We'll start with a calculator, small enough to follow all the way from source
text to an answer. We'll build its parser twice. LALRPOP lets us write down a
grammar and generates the parsing machinery for us. A Pratt parser puts the
operator-handling logic in a short Rust function. Seeing both is a useful way
to discover what a parser generator is doing on our behalf.

## The Language, Now

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

There are two plausible trees for `1 + 2 * 3`. One adds first and returns 9.
The other multiplies first and returns 7. We want the second, so our grammar
needs to express that choice. We'll separate sums, products, and atoms into
different rules. A sum can contain a whole product as an operand. Parentheses
let us start again from the full expression grammar when we want another
grouping.

### Choosing a parsing strategy

There isn't one parsing technique that wins at everything. Some approaches
make the whole grammar easy to inspect. Others make it easy to write a little
parser directly in the host language. Before choosing, it helps to separate
three things we often bundle together. The grammar says what syntax we accept,
the algorithm recognizes it, and the API determines how we write the parser.

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
A conflict is useful feedback. The grammar needs attention before we can trust
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
parsing expression grammars (PEGs), whose alternatives use ordered choice. The
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

We don't need an inferencer yet, since integers are our only values.
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

`main.rs` connects these pieces to commands and a REPL. Try both parsers on
the same expression and inspect their trees. The tests do this too, including
generated expressions with different groupings. Comparing the trees catches
mistakes that a coincidentally correct numerical answer could hide.

## Running It

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

## Further Reading

Pierce separates syntax and semantics carefully [@pierce2002]. The Haskell
Report specifies a much larger concrete syntax and fixity-resolution procedure
[@haskell2010].
