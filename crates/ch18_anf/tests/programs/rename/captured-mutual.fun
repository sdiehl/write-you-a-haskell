f x = let { g y = if y == 0 then x else h (y-1); h y = g y; } in g 10
_old = f 42

main = print _old
