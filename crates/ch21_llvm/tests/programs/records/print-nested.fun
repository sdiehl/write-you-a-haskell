data P = P { x :: Int, y :: Int }
_old = Just (P 3 4)

main = print _old

instance Show P where { show (P x y) = "P {x = " <> show x <> ", y = " <> show y <> "}" }
