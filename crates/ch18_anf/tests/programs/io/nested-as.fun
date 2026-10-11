main = do { Just (whole@(x:xs)) <- pure (Just [1,2]); print (whole,x,xs) }
