data P = P { x :: Int, y :: Int }
data Q = Q { x :: String }
set :: P -> P
set p = p {x = 10}
_old = set (P 3 4)

main = print _old

instance Show P where { show (P x y) = "P {x = " <> show x <> ", y = " <> show y <> "}" }
