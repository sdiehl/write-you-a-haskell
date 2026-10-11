main = do
  x <- do
    putStrLn "inner"
    pure 42
  print x
