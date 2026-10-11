make s = \_ -> putStrLn s
main = do
  let say = make "λ🙂"
  say ()
  say ()
