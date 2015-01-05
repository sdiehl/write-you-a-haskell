module Eval (
  eval
) where

import Syntax

import Data.Maybe
import Data.Functor

-- Evaluate a single step.
eval1 :: Expr -> Maybe Expr
eval1 expr = case expr of
  IsZero Zero               -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t                  -> IsZero <$> (eval1 t)
  Succ t                    -> Succ <$> (eval1 t)
  Pred Zero                 -> Just Zero
  Pred (Succ t) | isNum t   -> Just t
  Pred t                    -> Pred <$> (eval1 t)
  If Tr  c _                -> Just c
  If Fl _ a                 -> Just a
  If t c a                  -> (\t' -> If t' c a) <$> eval1 t
  _                         -> Nothing

isNum :: Expr -> Bool
isNum Zero     = True
isNum (Succ t) = isNum t
isNum _        = False

isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal t | isNum t = True
isVal _ = False

nf :: Expr -> Expr
nf t = fromMaybe t (nf <$> eval1 t)

eval :: Expr -> Maybe Expr
eval t = case isVal (nf t) of
  True  -> Just (nf t)
  False -> Nothing -- term is "stuck"
