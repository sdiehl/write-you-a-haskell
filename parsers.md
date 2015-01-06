![](img/titles/parsing.png)

******

<!--
<blockquote>
The tools we use have a profound (and devious!) influence on our thinking habits, and, therefore, on our thinking abilities.
<cite>
Edsger Dijkstra</cite>
</blockquote>
-->

<p class="halfbreak">
</p>

Parser Combinators
==================

For parsing in Haskell it is quite common to use a family of libraries known as
*parser combinators* which let us compose higher order functions to generate
parsers. Parser combinators are a particularly expressive pattern that allows us
to quickly prototype language grammars in an small embedded domain language
inside of Haskell itself. Most notably we can embed custom Haskell logic inside
of the parser.

NanoParsec
----------

So now let's build our own toy parser combinator library which we'll call
**NanoParsec** just to get the feel of how these things are built.

~~~~ {.haskell slice="chapter3/parsec.hs" lower=1 upper=8}
~~~~

Structurally a parser is a function which takes an input stream of characters
and yields an parse tree by applying the parser logic over sections of the
character stream (called *lexemes*) to build up a composite data structure for
the AST.

~~~~ {.haskell slice="chapter3/parsec.hs" lower=9 upper=9}
~~~~

Running the function will result in traversing the stream of characters yielding
a resultant AST structure for the type variable ``a``, or failing with a parse
error for malformed input, or failing by not consuming the entire stream of
input. A more robust implementation would track the position information of
failures for error reporting.

~~~~ {.haskell slice="chapter3/parsec.hs" lower=11 upper=16}
~~~~

Recall that in Haskell in the String type is itself defined to be a list of
``Char`` values, so the following are equivalent forms of the same data.

```haskell
"1+2*3"
['1', '+', '2', '*', '3']
```

We advance the parser by extracting a single character from the parser stream
and returning in a tuple containing itself and the rest of the stream. The
parser logic will then scrutinize the character and either transform it in some
portion of the output or advance the stream and proceed.

~~~~ {.haskell slice="chapter3/parsec.hs" lower=18 upper=22}
~~~~

A bind operation for our parser type will take one parse operation and compose
it over the result of second parse function. Since the parser operation yields a
list of tuples, composing a second parser function simply maps itself over the
resulting list and concat's the resulting nested list of lists into a single
flat list in the usual list monad fashion. The unit operation injects a single
pure value into the parse stream.

~~~~ {.haskell slice="chapter3/parsec.hs" lower=24 upper=28}
~~~~

As the terminology might have indicated this is indeed a Monad (also Functor and
Applicative).

~~~~ {.haskell slice="chapter3/parsec.hs" lower=30 upper=39}
~~~~

Of particular importance is that this particular monad has a zero value
(``failure``), namely the function which halts reading the stream and returns
the empty stream.  Together this forms a monoidal structure with a secondary
operation (``combine``) which applies two parser functions over the same stream
and concatenates the result. Together these give rise to both the Alternative
and MonadPlus class instances which encode the logic for trying multiple parse
functions over the same stream and handling failure and rollover.

The core operator introduced here is (``(<|>)``) operator for combining two
optional paths of parser logic, switching to second path if the first fails with
the zero value.

~~~~ {.haskell slice="chapter3/parsec.hs" lower=41 upper=59}
~~~~

Derived automatically from the Alternative typeclass definition is the ``many``
and ``some`` functions. Many takes a single function argument and repeatedly
applies it until the function fails and then yields the collected results up to
that point. The ``some`` function behaves similar except that it will fail
itself if there is not at least a single match.

```haskell
-- | One or more.
some :: f a -> f [a]
some v = some_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

-- | Zero or more.
many :: f a -> f [a]
many v = many_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v
```

On top of this we can add functionality for checking whether the current
character in the stream matches a given predicate ( i.e is it a digit, is it a
letter, a specific word, etc).

~~~~ {.haskell slice="chapter3/parsec.hs" lower=61 upper=65}
~~~~

Essentially this 50 lines code encodes the entire core of the parser combinator
machinery. All higher order behavior can be written on top of just this logic.
Now we can write down several higher level functions which operate over sections
of the stream.

``chainl1`` parses one or more occurrences of ``p``, separated by ``op`` and
returns a value obtained by a recursing until failure on the left hand side of
the stream. This can be used to parse left-recursive grammar.

~~~~ {.haskell slice="chapter3/parsec.hs" lower=71 upper=82}
~~~~

Using ``satisfy`` we can write down several combinators for detecting the
presence of specific common patterns of characters ( numbers, parenthesized
expressions, whitespace, etc ).

~~~~ {.haskell slice="chapter3/parsec.hs" lower=84 upper=117}
~~~~

**And that's about it!** In a few hundred lines we have enough of a parser
library to write down a simple parser for a calculator grammar. In the formal
Backus–Naur Form our grammar would be written as:

```haskell
number = [ "-" ] digit { digit }.
digit  = "0" | "1" | ... | "8" | "9".
expr   = term { addop term }.
term   = factor { mulop factor }.
factor = "(" expr ")" | var | number.
addop  = "+" | "-".
mulop  = "*".
```

The direct translation to Haskell in terms of our newly constructed parser
combinator has the following form:

~~~~ {.haskell slice="chapter3/parsec.hs" lower=131 upper=183}
~~~~

Now we can try out our little parser.

```bash
$ runhaskell parsec.hs
> 1+2
3
> 1+2*3
7
```

See:

* [Monads for functional programming](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)

**Generalizing String**

The limitations of the String type are well-known, but what is particularly nice
about this approach is that it is adapts to different stream type simply by
adding an additional parameter to the Parser type which holds the stream type.
In place a more efficient data structure like ``Data.Text`` can replaced.

```haskell
newtype Parser s a = Parser { parse :: s -> [(a,s)] }
```

For the first couple of simple parsers we will use the String type for
simplicity's sake, but later will generalize our parsers to use the ``Text``
type. The combinators and parsing logic will not change, only the lexer and
language definitions types will change slightly to a generalized form.

Parsec
------

Now that we have the feel for parser combinators work, we can graduate to full
the Parsec library. We'll effectively ignore the gritty details of parsing and
lexing from here out. Although an interesting subject parsing is effectively a
solved problem and the details are not terribly important for our purposes.

The *Parsec* library defines a set of common combinators much like the operators
we defined in our toy library.

              Combinators   
-----------   ------------
``<|>``       The choice operator tries to parse the first argument before proceeding to the second. Can be chained sequentially to a generate a sequence of options.
``many``      Consumes an arbitrary number of patterns matching the given pattern and returns them as a list.
``many1``     Like many but requires at least one match. 
``optional``  Optionally parses a given pattern returning its value as a Maybe.
``try``       Backtracking operator will let us parse ambiguous matching expressions and restart with a different pattern.
``parens``    Parsers the given pattern surrounded by parentheses.

**Tokens**

To create a Parsec lexer we must first specify several parameters about how
individual characters are handled and converted into tokens. For example some
tokens will be handled as comments and simply omitted from the parse stream.
Other parameters include indicating what characters are to be handled as keyword
identifiers or operators.

```haskell
langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = reservedNames
  , Tok.reservedOpNames = reservedOps
  , Tok.caseSensitive   = True
  }
```

**Lexer**

Given the token definition we can create the lexer functions.

~~~~ {.haskell slice="chapter3/calc/Parser.hs" lower=31 upper=47}
~~~~

**Abstract Syntax Tree**

In a separate module we'll now define the abstract syntax for our language as a
datatype.

~~~~ {.haskell include="chapter3/calc/Syntax.hs"}
~~~~

**Parser**

Much like before our parser is simply written in monadic blocks, each mapping a
a set of patterns to a construct in our ``Expr`` type. The toplevel entry point
to our parser is the ``expr`` function which we can parse with by using the
Parsec function ``parse``.

~~~~ {.haskell slice="chapter3/calc/Parser.hs" lower=46 upper=94}
~~~~

The toplevel function we'll expose from our Parse module is the ``parseExpr``
which will be called as the entry point in our REPL.

~~~~ {.haskell slice="chapter3/calc/Parser.hs" lower=99 upper=100}
~~~~

Evaluation
----------

Our small language gives rise has two syntactic classes, values and expressions.
Values are in *normal form* and cannot be reduced further. These consist of
``True`` and ``False`` values and literal numbers. 

~~~~ {.haskell slice="chapter3/calc/Eval.hs" lower=8 upper=17}
~~~~

The evaluation of our languages uses the ``Maybe`` applicative to accommodate
the fact that our reduction may halt at any level with a Nothing if the
expression being reduced has reached a normal form or cannot proceed because the
reduction simply isn't well-defined. The rules for evaluation are a single step
by which an expression takes a single small step one from form to another by a
given rule.

~~~~ {.haskell slice="chapter3/calc/Eval.hs" lower=19 upper=31}
~~~~

At the toplevel we simply apply the ``nf`` repeatedly until either a value is
reached or we're left with an expression that has no well-defined way to
proceed. The term is "stuck" and the program is an undefined state.

~~~~ {.haskell slice="chapter3/calc/Eval.hs" lower=33 upper=39}
~~~~

REPL
----

The driver for our simple language simply invokes all of the parser and
evaluation logic in a loop feeding the resulting state to the next iteration. We
will use the [haskeline](http://hackage.haskell.org/package/haskeline) library
to give us readline interactions for the small REPL. Behind the scenes haskeline
is using readline or another platform-specific system library to manage the
terminal input. To start out we just create the simplest loop, which only parses
and evaluates expressions and prints them to the screen. We'll build on this
pattern in each chapter, eventually ending up with a more full-featured REPL.

The two functions of note are the operations for the ``InputT`` monad
transformer.

```haskell
runInputT :: Settings IO -> InputT IO a -> IO a
getInputLine :: String -> InputT IO (Maybe String)
```

When the user enters a ``EOF`` or sends a ``SIGQUIT`` to input, ``getInputLine``
will yield ``Nothing`` and can handle the exit logic.

```haskell
process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> print $ runEval ex

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Repl> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
```

Soundness
---------

Great, now let's test our little interpreter and indeed we see that it behaves
as expected.

```bash
Arith> succ 0
succ 0

Arith> succ (succ 0)
succ (succ 0)

Arith> iszero 0
true

Arith> if false then true else false
false

Arith> iszero (pred (succ (succ 0)))
false

Arith> pred (succ 0)
0

Arith> iszero false
Cannot evaluate

Arith> if 0 then true else false
Cannot evaluate
```

Oh no, our calculator language allows us to evaluate terms which are
syntactically valid but semantically meaningless. We'd like to restrict the
existence of such terms since when we start compiling our languages later into
native CPU instructions these kind errors will correspond to all sorts of
nastiness (segfaults, out of bounds errors, etc). How can we make these illegal
states unrepresentable to begin with?

Full Source
-----------

* [NanoParsec](https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter3/parsec.hs)
* [Calculator](https://github.com/sdiehl/write-you-a-haskell/tree/master/chapter3/calc)
