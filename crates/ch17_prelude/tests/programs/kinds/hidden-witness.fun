data Hold f = Hold (f Int)
ignore x = 42
_old = ignore Hold

main = print _old
