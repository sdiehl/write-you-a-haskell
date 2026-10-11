data P = P { x :: Int, y :: Int }
main = P {y = 1 `div` 0, x = case Nothing of { Just n -> n; }}
