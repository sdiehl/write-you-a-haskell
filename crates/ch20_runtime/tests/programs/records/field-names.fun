data P = P { x, y :: Int }
_old = P {x = 3,y = 4}

main = print _old

instance Show P where { show (P x y) = "P {x = " <> show x <> ", y = " <> show y <> "}" }
