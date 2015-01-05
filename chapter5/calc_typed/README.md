Arith
======

A typed arithmetic.

To compile and run:

```shell
$ cabal run
```

Usage:

```ocaml
Arith> succ 0
succ 0 : Nat

Arith> succ (succ 0)
succ (succ 0) : Nat

Arith> if false then true else false
false : Bool

Arith> iszero (pred (succ (succ 0)))
false : Bool

Arith> pred (succ 0)
0 : Nat

Arith> iszero false
Type Mismatch: Bool is not Nat

Arith> if 0 then true else false
Type Mismatch: Nat is not Bool
```

License
=======

Released under MIT license.
