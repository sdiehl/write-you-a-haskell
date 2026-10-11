class Keep a where
  keep :: a -> b -> a
data Box a = Box a
instance Keep (Box b) where
  keep x y = x
main = keep (Box 42) True
