reverse xs = go xs [] where { go [] acc = acc; go (x:xs) acc = go xs (x:acc); }
main = reverse [1,2,3]
