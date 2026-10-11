foldr f z [] = z
foldr f z (x : xs) = f x (foldr f z xs)
_old = foldr (+) 0 [1,2,3,4]

main = print _old
