data T = T String Char [Char] Double deriving Show
main = print (T "hello world" ' ' ['a','b'] (-0.0))
