foldr f z [] = z
foldr f z (x : xs) = f x (foldr f z xs)
main = foldr (+) 0 [1,2,3,4]
