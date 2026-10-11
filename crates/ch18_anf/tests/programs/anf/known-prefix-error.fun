f :: Int -> Int -> Int
f x = error "function first"
main = print (f 1 (error "argument second"))
