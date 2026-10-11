-- engines: native
evenN :: Int -> Bool
evenN 0 = True
evenN n = oddN (n - 1)
oddN :: Int -> Bool
oddN 0 = False
oddN n = evenN (n - 1)
main = print (evenN 10000000)
