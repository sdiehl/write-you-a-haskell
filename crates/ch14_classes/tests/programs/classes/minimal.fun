class Value a where
  value :: a -> Int

instance Value Int where
  value x = x + 1

main = value 41
