choose x = if x == 0 then (\a b -> a + b) else (\a b -> a - b)
main = print (choose 0 20 22)
