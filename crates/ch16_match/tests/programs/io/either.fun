main = print ((do { x <- Right 40; pure (x+2) }) :: Either String Int)
