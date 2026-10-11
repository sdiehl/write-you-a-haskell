data Pair = Pair Int Int
_old = case Pair (1 `div` 0) 1 of { _ -> 42; }

main = (\_ -> pure ()) _old
