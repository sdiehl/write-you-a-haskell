Arith
======

A untyped arithmetic.

To compile and run:

```shell
$ cabal run
```

Usage:

```ocaml
Arith> succ 0
succ 0

Arith> succ (succ 0)
succ (succ 0)

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

License
=======

Released under MIT license.
