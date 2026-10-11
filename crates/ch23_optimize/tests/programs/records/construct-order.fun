data P = P { x :: Int, y :: Int }
_old = P { y = 4, x = 3 }

main = print _old

instance Show P where { show (P x y) = "P {x = " <> show x <> ", y = " <> show y <> "}" }
