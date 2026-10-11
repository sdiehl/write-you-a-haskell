-- stderr: exact
main = let x = trace "once" 21 in print (x + x)
