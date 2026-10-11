data T a = Leaf a | Fork (T a) (T a) deriving (Show)
main = print (Fork (Leaf 1) (Leaf (-2)))
