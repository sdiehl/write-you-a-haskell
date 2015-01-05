module Pretty (
  ppexpr,
  pptype
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
  ppr p expr = case expr of
    Zero -> PP.text "0"
    Tr -> PP.text "true"
    Fl -> PP.text "false"
    Succ a -> (parensIf (p > 0) $ PP.text "succ" <+> ppr (p+1) a)
    Pred a -> (parensIf (p > 0) $ PP.text "succ" <+> ppr (p+1) a)
    IsZero a -> (parensIf (p > 0) $ PP.text "iszero" <+> ppr (p+1) a)
    If a b c ->
          PP.text "if"   <+> ppr p a
      <+> PP.text "then" <+> ppr p b
      <+> PP.text "else" <+> ppr p c

instance Pretty Type where
  ppr _ TNat = PP.text "Nat"
  ppr _ TBool = PP.text "Bool"

ppexpr :: Expr -> String
ppexpr = PP.render . ppr 0

pptype :: Type -> String
pptype = PP.render . ppr 0
