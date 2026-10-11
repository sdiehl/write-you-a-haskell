f x = let { g y = x; a = g 1; b = g True; } in b
main = f 42
