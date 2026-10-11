data Pair a b = Pair a b
instance Eq (Pair a b) where
  x == y = True
instance Eq (Pair b a) where
  x == y = False
main = 1
