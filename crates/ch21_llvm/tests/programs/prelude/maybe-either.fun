main = do { print (maybe 0 (+1) (Just 2), either length length (Left [1,2])) }
