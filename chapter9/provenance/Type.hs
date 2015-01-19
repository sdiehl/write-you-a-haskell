module Type (
  Type(..),
  TVar(..),
) where

import Syntax (Loc, Name)

data Type
  = TVar Loc TVar
  | TCon Loc Name
  | TArr Loc Type Type
  deriving (Show, Eq, Ord)

newtype TVar = TV String
  deriving (Show, Eq, Ord)
