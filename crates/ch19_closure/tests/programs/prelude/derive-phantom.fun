data Phantom a = Phantom deriving (Eq, Ord, Show)
main = print (Phantom :: Phantom (Int -> Int))
