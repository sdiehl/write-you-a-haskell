# Chapter 8: Layout and the Extended Parser

> **New files**
>
> - `src/layout.rs`
> - `src/fixity.rs`
> - `src/format.rs`
>
> **Changed files**
>
> - `src/parse.rs`
> - `src/typecheck.rs`
> - `src/ast.rs`
> - `src/elaborate.rs`
> - `src/lexer.rs`
> - `src/lib.rs`
> - `src/main.rs`
> - `src/types.rs`

## Motivation

The braces were useful while we were getting started. By now they're mostly
making us type punctuation for structure we can already see in the indentation.
Let's make Fun a little nicer to write.

We'll add layout, local helpers with `where`, type signatures, and user-defined
operators. Much of this can happen before evaluation. Indentation becomes
punctuation, sections become lambdas, and operator chains acquire their grouping
once we know the declared fixities. It's a satisfying compiler trick. We can
make the source language more expressive while keeping its evaluator small.

## The Language, Now

Braces and semicolons become optional when indentation supplies their boundaries.
A `where` block belongs to its equation. The equation's parameters remain visible
inside it. Several local declarations may share a `let` block.

```fun
addTwo x = x + y
  where
    y = 2
main = let result = addTwo 40 in result
```

Signatures can appear before or after their binding within the same block. Type
variables are implicitly quantified. A signature may specialize an inferred type,
but it cannot make the implementation more polymorphic than it is.

```fun
id :: a -> a
id x = x
main = let { n :: Int; n = id 42; } in n
```

Operators may be defined in prefix or infix form. Their fixities apply throughout
the file, even before the fixity declaration. An undeclared operator associates
to the left at precedence 9. Named functions become operators inside backticks.

```fun
infixr 5 %%
a %% b = a * 10 + b
main = 1 %% 2 %% 3
```

This program prints `33` because it means `1 %% (2 %% 3)`. Sections supply one argument:
`(40 +)` and `(+ 2)` are functions. `(- 2)` instead means negation. Unary minus
has precedence 6, so `-2 * 3 + 4` means `(-(2 * 3)) + 4`. Write parentheses around
negation used after an operator of precedence 6 or greater.

## Theory

Layout is easier to understand once we stop treating whitespace as mysterious.
We're going to translate indentation into the braces and semicolons our parser
already knows. The Haskell Report describes this translation [@haskell2010].
Let's do it by hand before looking at the adapter.

### See the Invisible Delimiters

Think of indentation as a shorthand for punctuation. The first declaration in a
block sets its left margin. Another declaration at that margin begins a new
item. Moving back to an enclosing margin finishes the block.

```fun
scale x = result
  where
    doubled = x + x
    result = doubled * 3
main = let n = 7 in scale n
```

Before parsing, layout makes the omitted punctuation explicit. Here every brace
and semicolon was inserted by the layout pass:

```fun
{ scale x = result
    where { doubled = x + x
          ; result = doubled * 3
          }
; main = let { n = 7 } in scale n
}
```

The file gets an outer pair of braces. `doubled` establishes the `where` block's
indentation, and `result` at the same column gets a separator before it. `main`
is farther left, so layout closes the `where` block and separates the next
top-level declaration. The same-line `in` closes the `let` block even though
there was no dedent. The final brace closes the file at end of input. Writing
the braces yourself produces the same grouping.

The later pattern and IO chapters use the same mechanism after `of` and `do`:

```fun
main = do
  case flag of
    True -> putStrLn "yes"
    False -> putStrLn "no"
  putStrLn "done"
```

Its explicit form is:

```fun
{ main = do
    { case flag of
        { True -> putStrLn "yes"
        ; False -> putStrLn "no"
        }
    ; putStrLn "done"
    }
}
```

The dedent before the last action closes only the `of` block. That action is
still inside `do`. These constructs arrive later, but the rule for translating
their indentation is already the same. Use `--dump=layout` to see the actual
tokens, including the spans that distinguish inserted punctuation from source
characters.

Fixity resolution compares adjacent operators before constructing applications.
A greater precedence binds first. Equal precedences require compatible
associativities. Two left-associative operators reduce from the left, and two
right-associative operators from the right. Non-associative operators and mixed
associativities at the same precedence require parentheses [@haskell2010].

A signature makes a promise. If we write `a -> a`, we mean the function works
for any `a` its caller chooses. The checker mustn't make that promise true by
quietly choosing `a = Int`.

We enforce this by replacing the signature's quantified variables with rigid
identities, called _skolems_. Inference variables can be solved, but a skolem
stands for an arbitrary type and must stay arbitrary. We instantiate the inferred
scheme with fresh flexible variables and try to unify it with the rigid signature.
We also check that no skolem escapes into the surrounding scope. Together these
checks tell us whether the inferred type supports the promise
[@damas1982; @pierce2002].

## Implementation

### Layout and trivia

The logos lexer emits comment tokens with their original text. `TriviaLexer`
records them in a side table and exposes only semantic tokens. `LayoutLexer`
then performs indentation insertion. The adapter groups explicit braces and
parentheses, recursively handling layout inside them, so a parenthesized `let`
still gets its own block.

```rust
{{#include ../../crates/ch08_layout/src/layout.rs:layout}}
```

The library does not expose a `let`/`in` closure operation. The adapter tracks which
virtual frames have already closed at `in`, suppressing their later separators
and closing braces. It also handles empty blocks and redundant virtual separators
beside explicit semicolons. Tokens keep their original byte offsets. Inserted
punctuation has zero-width spans.

`of` and `do` participate in lexical layout tests now. Their language constructs
still wait for Chapters 11 and 15, respectively. The compiler does not accept
those expressions prematurely.

### Operator chains

The grammar retains a first operand followed by operator/operand pairs. Parentheses
remain explicit until the resolver has established their boundary. A stack resolves
these chains. A distinguished unary entry gives negation its precedence.

```rust
{{#include ../../crates/ch08_layout/src/fixity.rs:chain}}
```

Sections become lambdas with generated names that source identifiers cannot spell.
The current primitive operators remain direct arithmetic nodes when unshadowed.
User operators become ordinary curried applications. Core therefore needs no new
expression forms.

### Checking signatures

Declarations are collected before signatures are attached to bindings. The checker
first infers and generalizes a binding, then tests its signature in a trial store.
A successful check specializes the live scheme used by subsequent occurrences.
Recursive calls remain monomorphic during inference. Signature-guided polymorphic
recursion arrives with the later signature checker.

```rust
{{#include ../../crates/ch08_layout/src/typecheck.rs:signature}}
```

### Formatting

The formatter uses marginalia's document algebra and attached comment map. Its
canonical output uses explicit blocks and keeps source parentheses. Comments move
to separate leading lines at token boundaries, preserving their text and order.
This avoids inserting punctuation inside a line comment.

```rust
{{#include ../../crates/ch08_layout/src/format.rs:format}}
```

The tests compare parsed trees after removing spans, rather than merely comparing
results of evaluation. Every syntactically valid corpus program must round-trip,
and formatting the output again must produce exactly the same bytes. Dedicated
cases retain nested block comments and comments beside `where`.

## Running It

```text
$ cd crates/ch08_layout
$ cargo run -q -- run tests/programs/layout/right-fixity.fun
33
$ cargo run -q -- dump --pass=fixity tests/programs/layout/negation.fun
{
  main = -(2 * 3) + 4;
}
$ cargo run -q -- fmt example.fun
$ cargo run -q -- fmt example.fun --check
```

The three dumps make a useful little tour. `--dump=layout` reveals the inserted
punctuation, `--dump=parse` shows operator chains before grouping, and
`--dump=fixity` shows the chosen grouping. Try changing only the indentation or
a fixity declaration and watch which stage first changes.

## Exercises

1. **Introductory.** predict the grouping of three operators with distinct fixities,
   then compare the fixity dump with your answer.
2. **Intermediate.** Explain why `(1 + 2 *)` is rejected and repair it with parentheses.
3. **Intermediate.** Add a formatter test with nested comments between an operator
   and its operand. Check both tree equality and comment retention.
4. **Advanced.** Construct a local signature whose skolem would escape through a
   captured lambda parameter. Trace the trial-store check that rejects it.
5. **Advanced.** Compare the restricted `in` rule with the Report's general
   `parse-error(t)` rule. Identify syntax for which the latter would do more work.

## Further Reading

The Report specifies layout and fixity resolution precisely [@haskell2010].
Damas and Milner establish the principal-type property used by signature checks
[@damas1982]. Pierce develops the distinction between polymorphic types and their
instances in a broader type-theoretic setting [@pierce2002].
