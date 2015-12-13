Happy
=====

A simple example use of the Happy/Alex toolchain to generate parsers.

To compile and run:

```shell
$ cabal run
```

Usage:

```ocaml
Happy> 42
[TokenNum 42]
42

Happy> (\x -> x) 1
[TokenLParen,TokenLambda,TokenSym "x",TokenArrow,TokenSym "x",TokenRParen,TokenNum 1]
1

Happy> \x -> x*x*x*x*x - x + 1
[TokenLambda,TokenSym "x",TokenArrow,TokenSym "x",TokenMul,TokenSym "x",TokenMul,TokenSym "x",TokenMul,TokenSym "x",TokenMul,TokenSym "x",TokenSub,TokenSym "x",TokenAdd,TokenNum 1]
<<closure>>
```

License
=======

Released under MIT license.
