f x | y < 0 = 0 | y == 5 = y where { y = x + 1; }
f x = x
main = (f 4, f 7)
