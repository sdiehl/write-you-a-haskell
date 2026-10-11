doubleLoop :: Double -> Int -> Double
doubleLoop acc 0 = acc
doubleLoop acc n = doubleLoop ((acc + 0.25) * 1.0) (n - 1)
main = print (doubleLoop 0.0 10000)
