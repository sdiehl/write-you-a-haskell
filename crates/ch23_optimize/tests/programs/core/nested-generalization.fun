f x = let { g y = x; a = g 1; b = g True; } in b
_old = f 42

main = print _old
