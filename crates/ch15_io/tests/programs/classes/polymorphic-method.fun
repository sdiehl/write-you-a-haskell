class Keep a where
  keep :: a -> b -> a
data Box a = Box a
instance Keep (Box b) where
  keep x y = x
_old = keep (Box 42) True

main = print _old

instance Show a => Show (Box a) where { show (Box x) = "Box " ++ _showArg (show x) }
