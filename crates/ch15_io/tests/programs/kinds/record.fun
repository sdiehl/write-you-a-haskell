data Box f a = Box { value :: f a }
_old = (Box {value = Just 41}) {value = Just 42}

main = putStrLn (case _old of { Box x -> "Box {value = " ++ show x ++ "}" })
