-- stderr: exact
f x y = x + y
main = print (f (trace "left" 20) (trace "right" 22))
