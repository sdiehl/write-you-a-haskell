f x | y < 0 = 0 | y == 5 = y where { y = x + 1; }
f x = x
_old = (f 4, f 7)

main = print _old
