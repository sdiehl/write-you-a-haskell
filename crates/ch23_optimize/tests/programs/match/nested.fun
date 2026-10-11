f (Just (Just x)) = x
f (Just Nothing) = 0
f Nothing = 1
main = print (f (Just (Just 42)))
