loop 0 xs = xs
loop n xs = loop (n - 1) (n : xs)
main = print (sum (loop 10000 []))
