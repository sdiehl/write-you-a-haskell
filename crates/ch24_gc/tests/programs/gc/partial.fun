add x y z = x <> y <> z
main = do
  let f = add "α"
  let g = f "β"
  print (length (map show [1..100]))
  putStrLn (g "γ")
