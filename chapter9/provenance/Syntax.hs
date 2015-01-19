module Syntax (
  Expr(..),
  Name,
  Loc(..),
) where

type Name = String

data Expr
  = Var Loc Name
  | App Loc Expr Expr
  | Lam Loc Name Expr
  | Lit Loc Int

data Loc = NoLoc | Located Int
  deriving (Show, Eq, Ord)
