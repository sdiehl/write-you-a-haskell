module Eval where

import Syntax

import Control.Monad.Identity
import qualified Data.Map as Map

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr (Eval.Scope)

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show VClosure{} = "<<closure>>"

type Evaluate t = Identity t
type Scope = Map.Map String Value

eval :: Eval.Scope -> Expr -> Identity Value
eval env expr = case expr of

  Lit (LInt x) -> return $ VInt (fromIntegral x)

  Lit (LBool x) -> return $ VBool x

  Var x -> return $ env Map.! x

  Lam x _ body -> return (VClosure x body env)

  App a b -> do
    x <- eval env a
    y <- eval env b
    apply x y

extend :: Scope -> String -> Value -> Scope
extend env v t = Map.insert v t env

apply :: Value -> Value -> Evaluate Value
apply (VClosure v t0 e) t1 = eval (extend e v t1) t0
apply _ _  = error "Tried to apply closure"

emptyScope :: Scope
emptyScope = Map.empty

runEval :: Expr -> Value
runEval x = runIdentity (eval emptyScope x)
