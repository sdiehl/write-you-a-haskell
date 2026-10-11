make x = let
  f 0 = x
  f n = g (n - 1)
  g 0 = x <> "!"
  g n = f (n - 1)
  in f
main = do
  let f = make "kept"
  print (length (map show [1..100]))
  putStrLn (f 101)
