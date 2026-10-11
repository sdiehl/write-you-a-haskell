data Box f a = Box { value :: f a }
main = (Box {value = Just 41}) {value = Just 42}
