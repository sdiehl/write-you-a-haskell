x = 100
f x = let { x = 42; } in x
_old = f x

main = print _old
