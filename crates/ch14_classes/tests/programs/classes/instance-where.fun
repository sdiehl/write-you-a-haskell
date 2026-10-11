data Box = Box Int
instance Show Box where
  show (Box x) = prefix ++ show x where { prefix = "Box "; }
main = show (Box 42)
