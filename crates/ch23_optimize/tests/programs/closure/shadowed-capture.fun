main = print (let { x=10; f y=x+y; g x=f x; } in g 32)
