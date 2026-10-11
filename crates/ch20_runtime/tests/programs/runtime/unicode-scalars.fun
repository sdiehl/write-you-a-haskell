main = do { print (length (unpack "é中🦀")); print (pack ['é','中','🦀']); print ("a" < "é"); print (ord (chr 1114111)); }
