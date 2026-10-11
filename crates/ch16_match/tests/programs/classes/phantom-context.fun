data Box a = Box
instance Show a => Show (Box a) where
  show x = "Box"
_old = show (Box :: Box Int)

main = print _old
