{-# Language FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language TypeSynonymInstances #-}

module Pretty (
  ppdecl,
  ppexpr,
  ppsignature,
  pptype
) where

import Type
import Syntax
import Infer

import Text.PrettyPrint

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc
  pp :: p -> Doc
  pp = ppr 0

instance Pretty Name where
  ppr _ x = text x

instance Pretty TVar where
  ppr _ (TV x) = text x

instance Pretty Type where
  ppr p (TArr _ a b) = (parensIf (isArrow a) (ppr p a)) <+> text "->" <+> ppr p b
    where
      isArrow TArr{} = True
      isArrow _ = False
  ppr p (TVar _ a) = ppr p a
  ppr _ (TCon _ a) = text a

instance Pretty Expr where
  ppr p (Var _ a) = ppr p a
  ppr p (App _ a b) = parensIf (p > 0) $ ppr (p+1) a <+> ppr p b
  ppr p (Lam _ a b) = text "\\" <> ppr p a <+> text  "->" <+> ppr p b
  ppr _ (Lit _ a) = int a

instance Pretty Loc where
  ppr p (NoLoc) = ""
  ppr p (Located n) = int n

instance Show TypeError where
  show (UnificationFail a la b lb) =
    concat [
      "Cannot unify types: \n\t"
    , pptype a
    , "\n\tIntroduced at: "
    , (pploc la)
    , "\nwith \n\t"
    , pptype b
    , "\n\tIntroduced at: "
    , (pploc lb)
    ]
  show (InfiniteType (TV a) la b) =
    concat [
      "Cannot construct the infinite type: "
      , a
      , " = "
      , pptype b
      , "\n\tIntroduced at: "
      , (pploc la)
    ]
  show (Ambigious cs) =
    concat ["Cannot not match expected type: '" ++ pptype a ++ "' with actual type: '" ++ pptype b ++ "'\n" | (a,b) <- cs]
  show (UnboundVariable a) = "Not in scope: " ++ a

pploc :: Loc -> String
pploc = render . ppr 0

pptype :: Type -> String
pptype = render . ppr 0

ppexpr :: Expr -> String
ppexpr = render . ppr 0

ppsignature :: (String, Type) -> String
ppsignature (a, b) = a ++ " : " ++ pptype b

ppdecl :: (String, Expr) -> String
ppdecl (a, b) = "let " ++ a ++ " = " ++ ppexpr b
