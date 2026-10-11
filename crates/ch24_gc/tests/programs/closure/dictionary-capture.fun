member x = \xs -> elem x xs
main = print (map (member "é") [["é"],["a"],[]])
