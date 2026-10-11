main = let { id :: a -> a; id x = x; n = id True; } in id 42
