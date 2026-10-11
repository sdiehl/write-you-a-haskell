f (Just n) | n < 0 = 1 | n == 0 = 2
f (Just n) = n
f Nothing = 4
main = (f (Just 9), f (Just 0), f Nothing)
