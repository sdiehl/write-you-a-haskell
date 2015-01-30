module Eval where

import Syntax

import Data.Maybe
import Data.Functor

isNum :: Expr -> Bool
isNum Zero     = True
isNum (Succ t) = isNum t
isNum _        = False

isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal t | isNum t = True
isVal _ = False

eval' :: Expr -> Maybe Expr
eval' x = case x of
  IsZero Zero               -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t                  -> IsZero <$> (eval' t)
  Succ t                    -> Succ <$> (eval' t)
  Pred Zero                 -> Just Zero
  Pred (Succ t) | isNum t   -> Just t
  Pred t                    -> Pred <$> (eval' t)
  If Tr  c _                -> Just c
  If Fl _ a                 -> Just a
  If t c a                  -> (\t' -> If t' c a) <$> eval' t
  _                         -> Nothing

nf :: Expr -> Expr
nf x = fromMaybe x (nf <$> eval' x)

eval :: Expr -> Maybe Expr
eval t = case nf t of
  nft | isVal nft -> Just nft
      | otherwise -> Nothing -- term is "stuck"
