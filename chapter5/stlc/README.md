Simply Typed Lambda Calculus
============================

Simply typed lambda calculus.

To compile and run:

```shell
$ cabal run
```

Usage:

```haskell
./stlc
Stlc> (\x : Int . \y : Int . y) 1 2
2

Stlc> (\x : (Int -> Int). x) (\x : Int . 1) 2
1

Stlc> (\x : Int . x) False
Couldn't match expected type 'Int' with actual type: 'Bool'

Stlc> (\x : Int . (\y : Int . x))
<<closure>>
```

License
=======

Released under MIT license.
