main = do { when False (putStrLn "bad"); unless False (putStrLn "ok"); sequence_ [print 1,print 2] }
