f :: Int -> Int -> Int
f x = error "function before second argument"
main = print (f 1 (error "second argument"))
