data Box = Box Int
instance Show Box where
  show (Box x) = prefix ++ show x where { prefix = "Box "; }
_old = show (Box 42)

main = print _old
