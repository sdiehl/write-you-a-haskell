main = do
  Just x <- pure (Nothing :: Maybe Int)
  print x
