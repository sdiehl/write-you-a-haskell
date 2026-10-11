apply f = f 1 2 3 4 5 6
main = print (apply (\a b c d e f -> a + b + c + d + e + f))
