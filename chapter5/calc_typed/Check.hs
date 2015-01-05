module Check (
  check
) where

import Type
import Syntax
import Pretty

import Control.Monad.Except

data TypeError
  = TypeMismatch Type Type

instance Show TypeError where
    show (TypeMismatch a b) = "Type Mismatch: " ++ pptype a ++ " is not " ++ pptype b

type Check a = Except TypeError a

typeof :: Expr -> Check Type
typeof expr = case expr of
  Tr   -> return TBool
  Fl   -> return TBool
  Zero -> return TNat

  Succ a -> do
    ta <- typeof a
    case ta of
      TNat -> return TNat
      _    -> throwError $ TypeMismatch ta TNat

  Pred a -> do
    ta <- typeof a
    case ta of
      TNat -> return TNat
      _    -> throwError $ TypeMismatch ta TNat

  IsZero a -> do
    ta <- typeof a
    case ta of
      TNat -> return TBool
      _    -> throwError $ TypeMismatch ta TNat

  If a b c -> do
    ta <- typeof a
    tb <- typeof b
    tc <- typeof c
    if ta /= TBool
    then throwError $ TypeMismatch ta TBool
    else
      if tb /= tc
      then throwError $ TypeMismatch ta tb
      else return tc

check :: Expr -> Either TypeError Type
check = runExcept . typeof
