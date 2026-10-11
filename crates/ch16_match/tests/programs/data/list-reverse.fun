reverse xs = go xs [] where { go [] acc = acc; go (x:xs) acc = go xs (x:acc); }
_old = reverse [1,2,3]

main = print _old
