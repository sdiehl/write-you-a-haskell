data Box = Box Int deriving (Show)
apply f x = f x
main = do
  print (Box (20 + 22))
  print (apply (\x -> x + 1) (20 + 21))
