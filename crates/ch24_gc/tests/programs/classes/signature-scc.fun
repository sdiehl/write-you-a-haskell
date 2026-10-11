first x y = x
f x = first x g
g :: Int -> (Int,Bool)
g y = (f 1,f True)
_old = g 0

main = print _old
