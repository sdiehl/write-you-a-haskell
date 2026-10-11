main = do
  whole@(Just x) <- pure (Just 42)
  print (whole,x)
