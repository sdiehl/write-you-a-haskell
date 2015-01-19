{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer (
  inferTop,
  TypeError(..),
  Env,
) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax
import Type

-------------------------------------------------------------------------------
-- Substitution
-------------------------------------------------------------------------------

type Unifier = (Subst, [Constraint])
type Constraint = (Type, Type)
type Env = [(Name, Type)]

extend :: (Name, Type) -> Env -> Env
extend xt env = xt : env

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon l a)       = TCon l a
  apply (Subst s) t@(TVar _ a) = Map.findWithDefault t a s
  apply s (TArr l t1 t2) = TArr l (apply s t1) (apply s t2)

  ftv TCon{}         = Set.empty
  ftv (TVar _ a)     = Set.singleton a
  ftv (TArr _ t1 t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Constraint where
   apply s (t1, t2) = (apply s t1, apply s t2)
   ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv   = foldr (Set.union . ftv) Set.empty

data TypeError
  = UnificationFail Type Loc Type Loc
  | InfiniteType TVar Loc Type
  | UnboundVariable String
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]

-------------------------------------------------------------------------------
-- Environment
-------------------------------------------------------------------------------

inEnv :: (Name, Type) -> Check a -> Check a
inEnv (x,t) = local (extend (x,t))

lookupVar :: Name -> Check Type
lookupVar x = do
  env <- ask
  case lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s  -> return s

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Loc -> Check Type
fresh l = do
  s <- get
  put s{count = count s + 1}
  return $ TVar l (TV (letters !! count s))

-------------------------------------------------------------------------------
-- Type Checker
-------------------------------------------------------------------------------

-- | Inference state
data InferState = InferState { count :: Int }
type Check =
  WriterT [Constraint]
    (StateT InferState
      (ExceptT TypeError (Reader Env)))

infer :: Expr -> Check Type
infer expr = case expr of
  Var l n -> do
    t <- lookupVar n
    return $ setLoc l t

  App l a b -> do
    ta <- infer a
    tb <- infer b
    tr <- fresh l
    unify ta (TArr l tb tr)
    return tr

  Lam l n a -> do
    tv <- fresh l
    ty <- inEnv (n, tv) (infer a)
    return (TArr l (setLoc l ty) tv)

  Lit l _ -> return (setLoc l typeInt)

-------------------------------------------------------------------------------
-- Constraint Solving
-------------------------------------------------------------------------------

type Solve = StateT Unifier (Except TypeError)

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runExcept (evalStateT solver st)
  where st = (emptySubst, cs)

-- Unification solver
solver :: Solve Subst
solver = do
  (su, cs) <- get
  case cs of
    [] -> return su
    ((t1, t2): cs0) -> do
      (su1, cs1)  <- unifies t1 t2
      put (su1 `compose` su, cs1 ++ (apply su1 cs0))
      solver

-- | Empty unifier
emptyUnifer :: Unifier
emptyUnifer = (emptySubst, [])

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

bind ::  TVar -> Type -> Solve Unifier
bind a t
  | eqLoc t a        = return (emptySubst, [])
  | occursCheck a t  = throwError $ InfiniteType a (getLoc t) t
  | otherwise        = return $ (Subst $ Map.singleton a t, [])

-- | Type variables equal up to location
eqLoc :: Type -> TVar -> Bool
eqLoc (TVar _ a) b = a == b
eqLoc _ _ = False

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unifies :: Type -> Type -> Solve Unifier
unifies t1 t2 | t1 == t2 = return emptyUnifer
unifies (TVar _ v) t = v `bind` t
unifies t (TVar _ v) = v `bind` t
unifies (TArr _ t1 t2) (TArr _ t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TCon _ a) (TCon _ b) | a == b = return emptyUnifer
unifies t1 t2 = throwError $ UnificationFail t1 (getLoc t1) t2 (getLoc t2)

unifyMany :: [Type] -> [Type] -> Solve Unifier
unifyMany [] [] = return emptyUnifer
unifyMany (t1 : ts1) (t2 : ts2) =
  do (su1,cs1) <- unifies t1 t2
     (su2,cs2) <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1, cs1 ++ cs2)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

-- | Unify two types
unify :: Type -> Type -> Check ()
unify t1 t2 = tell [(t1, t2)]

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

runCheck :: Env -> Check a -> Either TypeError (a, [Constraint])
runCheck env =
    flip runReader env
  . runExceptT
  . flip evalStateT (InferState 0)
  . runWriterT

inferTop :: Env -> Expr -> Either TypeError Type
inferTop env x = do
  (ty, cs) <- runCheck env (infer x)
  s <- runSolve cs
  return (apply s ty)
