infixr 0 $
($) f x = f x
main = (\f -> f 42) $ \x -> x
