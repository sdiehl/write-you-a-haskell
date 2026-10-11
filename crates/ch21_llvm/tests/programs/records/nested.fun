data Outer = Outer { inner :: P }
data P = P { x :: Int, y :: Int }
_old = let p = Outer (P 3 4) in (p.inner.x, p {inner = p.inner {x = 10}})

main = print _old

instance Show P where { show (P x y) = "P {x = " <> show x <> ", y = " <> show y <> "}" }
instance Show Outer where { show (Outer p) = "Outer {inner = " <> show p <> "}" }
