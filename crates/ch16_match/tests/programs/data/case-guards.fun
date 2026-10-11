_old = case Just 3 of
  Just x | y == 9 -> y where { y = x * x; }
  _ -> 0

main = print _old
