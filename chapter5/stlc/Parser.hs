module Parser (
  parseExpr
) where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex

import Lexer
import Syntax

-------------------------------------------------------------------------------
-- Expression
-------------------------------------------------------------------------------

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  n <- natural
  return (Lit (LInt (fromIntegral n)))

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  x <- identifier
  reservedOp ":"
  t <- type'
  reservedOp "."
  e <- expr
  return (Lam x t e)

bool :: Parser Expr
bool =  (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

term :: Parser Expr
term =  parens expr
    <|> bool
    <|> number
    <|> variable
    <|> lambda

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

tyatom :: Parser Type
tyatom = tylit <|> (parens type')

tylit :: Parser Type
tylit = (reservedOp "Bool" >> return TBool) <|> (reservedOp "Int" >> return TInt)

type' :: Parser Type
type' = Ex.buildExpressionParser tyops tyatom
  where
    infixOp x f = Ex.Infix (reservedOp x >> return f)
    tyops = [
        [infixOp "->" TArr Ex.AssocRight]
      ]

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

parseExpr :: String -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input
