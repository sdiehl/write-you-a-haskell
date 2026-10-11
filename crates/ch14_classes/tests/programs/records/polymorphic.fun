data Box a = Box { value :: a }
get b = b.value
main = (get (Box 3),get (Box True),Box {value = "hi"})
