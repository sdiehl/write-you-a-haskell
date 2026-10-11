data P = P { x :: Int, y :: Int }
_old = (P 1 2) {y = 1 `div` 0, x = case Nothing of { Just n -> n; }}

main = (\_ -> pure ()) _old
