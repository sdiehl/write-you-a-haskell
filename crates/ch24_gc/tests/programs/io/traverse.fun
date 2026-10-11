main = do { mapM_ print [1,2]; xs <- mapM (\x -> pure (x+1)) [3,4]; print xs }
