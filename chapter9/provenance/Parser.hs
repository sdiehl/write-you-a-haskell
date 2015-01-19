{-# LANGUAGE OverloadedStrings #-}

module Parser (
  parseExpr,
  parseModule
) where

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Text.Lazy as L

import Lexer
import Syntax

import Control.Applicative ((<$>))

integer :: Parser Integer
integer = Tok.integer lexer

variable :: Parser Expr
variable = do
  x <- identifier
  l <- sourceLine <$> getPosition
  return (Var (Located l) x)

number :: Parser Expr
number = do
  n <- integer
  l <- sourceLine <$> getPosition
  return (Lit (Located l) (fromIntegral n))

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many identifier
  reservedOp "->"
  body <- expr
  l <- sourceLine <$> getPosition
  return $ foldr (Lam (Located l)) body args

aexp :: Parser Expr
aexp = parens expr
   <|> lambda
   <|> number
   <|> variable

expr :: Parser Expr
expr = do
  es <- many1 aexp
  l <- sourceLine <$> getPosition
  return (foldl1 (App (Located l)) es)

type Binding = (String, Expr)

val :: Parser Binding
val = do
  ex <- expr
  return ("it", ex)

top :: Parser Binding
top = do
  x <- val
  optional semi
  return x

modl ::  Parser [Binding]
modl = many top

parseExpr :: L.Text -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input

parseModule ::  FilePath -> L.Text -> Either ParseError [(String, Expr)]
parseModule fname input = parse (contents modl) fname input
