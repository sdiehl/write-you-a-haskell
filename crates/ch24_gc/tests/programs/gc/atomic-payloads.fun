data P = P Double String deriving (Show)
main = do
  let ps = map (\n -> P (toDouble n / 4.0) (show n <> "λ")) [1..8]
  print (length (unpack (foldr (<>) "" (map show [1..100]))))
  print ps
