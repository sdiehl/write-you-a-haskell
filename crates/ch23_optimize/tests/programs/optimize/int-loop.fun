intLoop :: Int -> Int -> Int
intLoop acc 0 = acc
intLoop acc n = intLoop (acc + 2305843009213693952) (n - 1)
main = print (intLoop 0 10002)
