{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}

module Pretty (
  ppconstraint,
  ppconstraints,
  ppdecl,
  ppenv,
  ppexpr,
  ppscheme,
  ppsubst,
  ppsignature,
  pptype
) where

import Env
import Type
import Syntax
import Infer

import Text.PrettyPrint hiding ((<>))
import qualified Data.Map as Map

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id


class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Name where
  ppr _ x = text x

instance Pretty TVar where
  ppr _ (TV x) = text x

instance Pretty Type where
  ppr p (TArr a b) = (parensIf (isArrow a) (ppr p a)) <+> text "->" <+> ppr p b
    where
      isArrow TArr{} = True
      isArrow _ = False
  ppr p (TVar a) = ppr p a
  ppr _ (TCon a) = text a

instance Pretty Scheme where
  ppr p (Forall [] t) = ppr p t
  ppr p (Forall ts t) = text "forall" <+> (hcat (punctuate space (map (ppr p) ts)) <> (text "." <+> ppr p t))

instance Pretty Binop where
  ppr _ Add = text "+"
  ppr _ Sub = text "-"
  ppr _ Mul = text "*"
  ppr _ Eql = text "=="

instance Pretty Expr where
  ppr p (Var a) = ppr p a
  ppr p (App a b) = parensIf (p > 0) $ ppr (p+1) a <+> ppr p b
  ppr p (Lam a b) = text "\\" <> (ppr p a <+> text  "->" <+> ppr p b)
  ppr p (Let a b c) = text "let" <> (ppr p a <+> text  "=" <+> ppr p b <+> text "in" <+> ppr p c)
  ppr p (Lit a) = ppr p a
  ppr p (Op o a b) = parensIf (p>0) $ ppr p a <+> ppr p o <+> ppr p b
  ppr p (Fix a) = parensIf (p>0) $ text "fix" <> ppr p a
  ppr p (If a b c) =
    text "if" <>
      (ppr p a <+>
       text "then" <+> ppr p b <+>
       text "else" <+> ppr p c)

instance Pretty Lit where
  ppr _ (LInt i) = integer i
  ppr _ (LBool True) = text "True"
  ppr _ (LBool False) = text "False"

instance Pretty Constraint where
  ppr p (a, b) = (ppr p a) <+> text " ~ " <+> (ppr p b)

instance Pretty [Constraint] where
  ppr p cs = vcat (punctuate space (map (ppr p) cs))

instance Pretty Subst where
  ppr p (Subst s) = vcat (punctuate space (map pprSub $ Map.toList s))
    where pprSub (a, b) = ppr 0 a <+> text "~" <+> ppr 0 b

instance Show TypeError where
  show (UnificationFail a b) =
    concat ["Cannot unify types: \n\t", pptype a, "\nwith \n\t", pptype b]
  show (InfiniteType (TV a) b) =
    concat ["Cannot construct the infinite type: ", a, " = ", pptype b]
  show (Ambigious cs) =
    concat ["Cannot not match expected type: '" ++ pptype a ++ "' with actual type: '" ++ pptype b ++ "'\n" | (a,b) <- cs]
  show (UnboundVariable a) = "Not in scope: " ++ a

ppscheme :: Scheme -> String
ppscheme = render . ppr 0

pptype :: Type -> String
pptype = render . ppr 0

ppexpr :: Expr -> String
ppexpr = render . ppr 0

ppsignature :: (String, Scheme) -> String
ppsignature (a, b) = a ++ " : " ++ ppscheme b

ppdecl :: (String, Expr) -> String
ppdecl (a, b) = "let " ++ a ++ " = " ++ ppexpr b

ppenv :: Env -> [String]
ppenv (TypeEnv env) = map ppsignature $ Map.toList env

ppconstraint :: Constraint -> String
ppconstraint = render . ppr 0

ppconstraints :: [Constraint] -> String
ppconstraints = render . ppr 0

ppsubst :: Subst -> String
ppsubst = render . ppr 0
