data Pair = Pair Int Int
main = case Pair (1 `div` 0) 1 of { _ -> 42; }
