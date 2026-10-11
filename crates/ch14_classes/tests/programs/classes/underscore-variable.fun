class Keep _a where { keep :: _a -> _a }
instance Keep Int where { keep x=x }
main=keep 42
