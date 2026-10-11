f (Just x) | x < 0 = 0
f (Just x) = x
f Nothing = 99
main = print (f (Just 42))
