infixr 0 $
($) f x = f x
_old = (\f -> f 42) $ \x -> x

main = print _old
