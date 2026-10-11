data P = P { x :: Int, y :: Int }
_old = (P 3 4) { y = 9, x = 8 }

main = print _old

instance Show P where { show (P x y) = "P {x = " ++ show x ++ ", y = " ++ show y ++ "}" }
