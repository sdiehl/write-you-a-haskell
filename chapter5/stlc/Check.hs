module Check (
  check,
  checkTop,
  TypeError(..)
) where

import Syntax
import Control.Monad.Except
import Control.Monad.Reader

type Env = [(Name, Type)]

extend :: (Name, Type) -> Env -> Env
extend xt env = xt : env

data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope Name

type Check = ExceptT TypeError (Reader Env)

inEnv :: (Name, Type) -> Check a -> Check a
inEnv (x,t) = local (extend (x,t))

lookupVar :: Name -> Check Type
lookupVar x = do
  env <- ask
  case lookup x env of
    Just e  -> return e
    Nothing -> throwError $ NotInScope x

check :: Expr -> Check Type
check expr = case expr of

  Lit LInt{} -> return TInt

  Lit LBool{} -> return TBool

  Lam x t e -> do
    rhs <- inEnv (x,t) (check e)
    return (TArr t rhs)

  App e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    case t1 of
       (TArr a b) | a == t2 -> return b
                  | otherwise -> throwError $ Mismatch t2 a
       ty -> throwError $ NotFunction ty

  Var x -> lookupVar x

runCheck :: Env -> Check a -> Either TypeError a
runCheck env = flip runReader env . runExceptT

checkTop :: Env -> Expr -> Either TypeError Type
checkTop env x = runCheck env $ (check x)
