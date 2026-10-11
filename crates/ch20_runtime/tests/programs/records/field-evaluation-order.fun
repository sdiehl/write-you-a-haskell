data P = P { x :: Int, y :: Int }
_old = P {y = 1 `div` 0, x = case Nothing of { Just n -> n; }}

main = (\_ -> pure ()) _old
