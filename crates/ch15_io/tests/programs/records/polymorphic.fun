data Box a = Box { value :: a }
get b = b.value
_old = (get (Box 3),get (Box True),Box {value = "hi"})

main = print _old

instance Show a => Show (Box a) where { show (Box x) = "Box {value = " ++ show x ++ "}" }
