-- engines: native
data Seven = Seven Int Int Int Int Int Int Int
loop :: Int -> Int -> Int
loop 0 acc = acc
loop n acc = case Seven n n n n n n n of
  Seven x _ _ _ _ _ _ -> loop (n - 1) (acc + x)
main = print (loop 12500000 0)
