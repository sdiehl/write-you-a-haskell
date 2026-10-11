make xs = \n -> length xs + n
main = do
  let f = make [1,2,3]
  print (length (map show [1..100]))
  print (f 39)
