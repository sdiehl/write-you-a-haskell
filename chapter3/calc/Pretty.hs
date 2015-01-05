module Pretty (
  ppexpr
) where

import Syntax
import Type

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

parensIf ::  Bool -> Doc -> Doc
parensIf True = PP.parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Expr where
  ppr _ Zero = PP.text "0"
  ppr _ Tr = PP.text "true"
  ppr _ Fl = PP.text "false"
  ppr p (Succ a) = (parensIf (p > 0) $ PP.text "succ" <+> ppr (p+1) a)
  ppr p (Pred a) = (parensIf (p > 0) $ PP.text "succ" <+> ppr (p+1) a)
  ppr p (IsZero a) = (parensIf (p > 0) $ PP.text "iszero" <+> ppr (p+1) a)
  ppr p (If a b c) =
        PP.text "if"   <+> ppr p a
    <+> PP.text "then" <+> ppr p b
    <+> PP.text "else" <+> ppr p c

ppexpr :: Expr -> String
ppexpr = PP.render . ppr 0
