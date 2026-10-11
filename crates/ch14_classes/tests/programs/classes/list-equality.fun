elem x [] = False
elem x (y:ys) = x == y || elem x ys
main = (elem 2 [1,2,3], elem True [False])
