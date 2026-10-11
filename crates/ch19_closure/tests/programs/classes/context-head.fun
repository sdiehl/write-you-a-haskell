data Box a = Box a
instance Eq b => Eq (Box a) where
  x == y = True
main = 1
