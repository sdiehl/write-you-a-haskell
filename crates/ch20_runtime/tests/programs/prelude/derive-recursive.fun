data T = Z | S T deriving (Eq, Ord, Show)
main = print (S (S Z) == S (S Z), compare Z (S Z))
