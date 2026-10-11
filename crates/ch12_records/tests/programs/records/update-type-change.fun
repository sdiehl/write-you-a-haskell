data Box a = Box {value :: a}
main = let b = Box 1 in b {value = True}
