data Tree a = Empty | Node (Tree a) a (Tree a)
insert x Empty = Node Empty x Empty
insert x (Node l y r) | x < y = Node (insert x l) y r
insert x (Node l y r) = Node l y (insert x r)
member x Empty = False
member x (Node l y r) | x == y = True | x < y = member x l
member x (Node l y r) = member x r
main = let t = insert 3 (insert 1 (insert 2 Empty)) in (member 3 t,member 5 t)
