module Eval (eval) where

import Syntax
import Control.Monad.State
import qualified Data.Map as Map

data Value
  = VInt Int
  | VUnit

instance Show Value where
  show (VInt x) = show x

type Eval = StateT Env IO
type Env = [(String, Value)]

eval1 :: Expr -> Eval Value
eval1 expr = case expr of
  Num a -> return (VInt a)
  Var a -> do
    env <- get
    case lookup a env of
      Just val -> return val
      Nothing -> error "Not in scope"
  Print a -> do
    a' <- eval1 a
    liftIO $ print a'
    return VUnit
  Assign ref val -> do
    modify $ \s -> (ref, VInt val) : s
    return VUnit

eval :: [Expr] -> IO ()
eval xs = evalStateT (mapM_ eval1 xs) []
