-- engines: anf
sumTo n = go 0 n where { go acc n = if n == 0 then acc else go (acc+n) (n-1); }
main = print (sumTo 10000)
