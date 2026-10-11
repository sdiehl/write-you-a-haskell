main = do { print (foldr (-) 0 [1,2,3], foldl (-) 0 [1,2,3], foldl' (+) 0 [1..4]) }
