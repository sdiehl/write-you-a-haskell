class Value a where { value :: a }
instance Value Int where { value = value }
main = 1
