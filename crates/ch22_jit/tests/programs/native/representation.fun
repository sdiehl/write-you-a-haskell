data T = Empty | Full Int String deriving (Show)
choose 0 = Empty
choose n = Full n "λ\0🦀"
main = do
  print (map choose [0,1,2])
  print [(-7) `div` 3, (-7) `mod` 3, 7 `div` (-3), 7 `mod` (-3)]
  print [(-4611686018427387904) `quot` (-1), 4611686018427387903 + 1]
