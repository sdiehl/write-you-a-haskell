{-# LANGUAGE OverloadedStrings #-}

module Type (
  -- * Types
  Type(..),
  Kind(..),
  TVar(..),
  Pred(..),
  TyCon(..),

  -- * Alpha equivalence
  Alpha(aeq),

  -- * Type predicates
  predicates,
  predicate,

  -- * Constructors
  mkTArr,
  mkTApp,
  mkTPair,
  mkTList,

  -- * Deconstructors
  viewTArr,
  viewTApp,
  typeArity,

  -- * Wired-in types
  tyArrow,
  tyList,
  tyPair,
  tyInt,
  tyChar,
  tyBool,
  tyUnit,
  tyAddr,

  intTyCon,
  charTyCon,
  addrTyCon,
  listTyCon,
  pairTyCon,
  unitTyCon,
) where

import Name
import Data.Char
import Data.String
import Data.List (foldl')

data Type
  = TVar TVar
  | TCon TyCon
  | TApp Type Type
  | TArr Type Type
  | TForall [Pred] [TVar] Type
  deriving (Show, Eq, Ord)

data Kind
  = KStar
  | KArr Kind Kind
  | KPrim
  | KVar Name
  deriving (Show, Eq, Ord)

data TyCon
  = AlgTyCon { tyId :: Name }
  | PrimTyCon { tyId :: Name }
  deriving (Show, Eq, Ord)

data Pred
  = IsIn Name Type
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Type Variables
-------------------------------------------------------------------------------

data TVar = TV
  { tvName   :: Name
  } deriving (Show, Eq, Ord)

instance IsString TVar where
  fromString x = TV (fromString x)

instance IsString TyCon where
  fromString = AlgTyCon . fromString

-------------------------------------------------------------------------------
-- Alpha Equivalence
-------------------------------------------------------------------------------

class Alpha a where
  aeq :: a -> a -> Bool

instance Alpha TVar where
  aeq _ _ = True

instance Alpha Type where
  aeq (TVar _) (TVar _)     = True
  aeq (TApp a b) (TApp c d) = aeq a c && aeq b d
  aeq (TArr a b) (TArr c d) = aeq a c && aeq b d
  aeq (TCon a) (TCon b)     = a == b
  aeq _ _                   = False

instance Alpha Kind where
  aeq KStar KStar = True
  aeq KPrim KPrim = True
  aeq (KArr a b) (KArr c d) = aeq a c && aeq b d
  aeq _ _ = False

-------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------

predicates :: Type -> [Pred]
predicates (TForall pd _ _) = pd

predicate :: [Pred] -> Type -> Type
predicate pd (TForall _ as ty) = TForall pd as ty

-------------------------------------------------------------------------------
-- Deconstructors
-------------------------------------------------------------------------------

viewTArr :: Type -> [Type]
viewTArr (TArr t1 t2) = t1 : viewTArr t2
viewTArr t = [t]


viewTApp :: Type -> [Type]
viewTApp t = go t []
  where
    go (TApp t1 t2) acc = go t1 (t2:acc)
    go t1 acc = (t1 : acc)

typeArity :: Type -> Int
typeArity ty = length (viewTArr ty)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

mkTArr :: [Type] -> Type
mkTArr []     = error "not defined for empty lists"
mkTArr [t]    = t
mkTArr (t:ts) = TArr t (mkTArr ts)

mkTApp :: TyCon -> [Type] -> Type
mkTApp tcon args = foldl' TApp (TCon tcon) args

mkTPair :: [Type] -> Type
mkTPair = foldr1 pair
  where pair x y = mkTApp (AlgTyCon "Pair") [x,y]

mkTList :: Type -> Type
mkTList tp
  = TApp (TCon (AlgTyCon "List")) tp

-------------------------------------------------------------------------------
-- Wired-in Types
-------------------------------------------------------------------------------

-- | @ Int# @
tyInt :: Type
tyInt = TCon intTyCon

-- | @ Char# @
tyChar :: Type
tyChar = TCon charTyCon

-- | @ Addr# @
tyAddr :: Type
tyAddr = TCon addrTyCon

-- | @ Bool @
tyBool :: Type
tyBool = TCon (AlgTyCon "Bool")

-- | @ \[\] @
tyList :: Type
tyList = TCon listTyCon

-- | @ (,) @
tyPair :: Type
tyPair = TCon pairTyCon

-- | @ () @
tyUnit :: Type
tyUnit = TCon unitTyCon


-- | Int#
intTyCon :: TyCon
intTyCon = PrimTyCon "Int"

-- | Char#
charTyCon :: TyCon
charTyCon = PrimTyCon "Char"

-- | Addr#
addrTyCon :: TyCon
addrTyCon = PrimTyCon "Addr"

-- | List
listTyCon :: TyCon
listTyCon = AlgTyCon "List"

-- | Pair
pairTyCon :: TyCon
pairTyCon = AlgTyCon "Pair"

unitTyCon :: TyCon
unitTyCon = AlgTyCon "Unit"

-- | (->)
tyArrow :: Type
tyArrow = TCon (AlgTyCon "->")
