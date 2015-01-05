module Pretty (
  ppexpr,
  pptype
) where

import Syntax
import Check

import Text.PrettyPrint

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

instance Pretty Expr where
  ppr p ex = case ex of
    Var x -> text x
    Lit (LInt a) -> text (show a)
    Lit (LBool b) -> text (show b)
    App a b -> (parensIf (p>0) (ppr (p+1) a)) <+> (ppr p b)
    Lam x t a -> parensIf (p > 0) $
          char '\\'
      <+> parens (text x <+> char ':' <+> ppr p t)
      <+> text "->"
      <+> ppr (p+1) a

instance Pretty Type where
  ppr _ TInt  = text "Int"
  ppr _ TBool = text "Bool"
  ppr p (TArr a b) = (parensIf (isArrow a) (ppr p a)) <+> text "->" <+> ppr p b
    where
      isArrow TArr{} = True
      isArrow _ = False

instance Show TypeError where
  show (Mismatch a b) =
    "Expecting " ++ (pptype b) ++ " but got " ++ (pptype a)
  show (NotFunction a) =
    "Tried to apply to non-function type: " ++ (pptype a)
  show (NotInScope a) =
    "Variable " ++ a ++ " is not in scope"

ppexpr :: Expr -> String
ppexpr = render . ppr 0

pptype :: Type -> String
pptype = render . ppr 0
