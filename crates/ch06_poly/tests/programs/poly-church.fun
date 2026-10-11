let two = \f -> \x -> f (f x) in two (\x -> x + 1) 40
