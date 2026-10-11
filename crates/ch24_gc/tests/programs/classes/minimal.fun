class Value a where
  value :: a -> Int

instance Value Int where
  value x = x + 1

_old = value 41

main = print _old
