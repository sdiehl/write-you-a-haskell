main = do { print ([1,2] >>= (\x -> [x,x+10])) }
