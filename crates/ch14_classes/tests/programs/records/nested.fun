data Outer = Outer { inner :: P }
data P = P { x :: Int, y :: Int }
main = let p = Outer (P 3 4) in (p.inner.x, p {inner = p.inner {x = 10}})
