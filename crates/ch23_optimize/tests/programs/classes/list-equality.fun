elem x [] = False
elem x (y:ys) = x == y || elem x ys
_old = (elem 2 [1,2,3], elem True [False])

main = print _old
