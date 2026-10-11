data Wrap f a = Wrap (f a)
un (Wrap x) = x
_old = un (Wrap (Just 42))

main = print _old
