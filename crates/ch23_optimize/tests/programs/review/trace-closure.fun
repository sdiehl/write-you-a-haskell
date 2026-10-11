-- stderr: exact
main = print ((trace "hello" (\x -> x + 1)) 2)
