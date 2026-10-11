data Box a = Box
instance Show a => Show (Box a) where
  show x = "Box"
main = show (Box :: Box Int)
