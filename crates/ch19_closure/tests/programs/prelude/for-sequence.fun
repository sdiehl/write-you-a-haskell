main = do { forM_ [1,2] print; sequence_ [putStr "a",putStrLn "b"] }
