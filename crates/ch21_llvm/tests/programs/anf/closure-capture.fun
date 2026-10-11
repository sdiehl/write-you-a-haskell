f x = let { g y = x+y; } in g
main = print (f 20 22)
