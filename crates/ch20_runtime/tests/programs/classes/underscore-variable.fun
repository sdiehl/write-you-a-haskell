class Keep _a where { keep :: _a -> _a }
instance Keep Int where { keep x=x }
_old=keep 42

main = print _old
