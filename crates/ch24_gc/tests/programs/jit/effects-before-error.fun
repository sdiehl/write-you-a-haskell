main = do
  _ <- putStrLn "before"
  print (42 `div` 0)
