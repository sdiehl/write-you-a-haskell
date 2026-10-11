data P = P {x :: Int}
main = do { p@P {x = n} <- pure (P 42); print (n,p.x) }
