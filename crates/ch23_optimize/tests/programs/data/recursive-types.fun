data A = A B | End
data B = B A
_old = A (B End)

main = print _old

instance Show A where { show End = "End"; show (A b) = "A " <> _showArg (show b) }
instance Show B where { show (B a) = "B " <> _showArg (show a) }
