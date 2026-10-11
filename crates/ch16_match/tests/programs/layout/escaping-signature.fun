f x = let { g :: a -> a; g y = x; } in g 1
main = f 42
