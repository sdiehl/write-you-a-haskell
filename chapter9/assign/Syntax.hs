module Syntax where

data Expr
  = Var String
  | Num Int
  | Print Expr
  | Assign String Int
  deriving (Eq,Show)
