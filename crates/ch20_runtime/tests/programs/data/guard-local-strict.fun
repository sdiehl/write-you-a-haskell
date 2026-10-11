f x | True = 1 where { unused = 1 `div` 0; }
_old = f 2

main = (\_ -> pure ()) _old
