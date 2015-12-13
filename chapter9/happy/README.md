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
Tokens: [TokenNum 42]
Syntax: Right (Lit (LInt 42))
42

Happy> (\x -> x) 1
Tokens: [TokenLParen,TokenLambda,TokenSym "x",TokenArrow,TokenSym "x",TokenRParen,TokenNum 1]
Syntax: Right (App (Lam "x" (Var "x")) (Lit (LInt 1)))
1
```

License
=======

Released under MIT license.
