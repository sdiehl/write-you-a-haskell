-- stderr: exact
f x = trace "call" (x + 1)
main = print (f 20 + f 20)
