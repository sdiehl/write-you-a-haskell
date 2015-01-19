{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (foldl1')

data Name
  = Gen Int
  | Name T.Text
  deriving (Eq, Show, Ord)

data Expr
  = Var Name
  | App Expr Expr
  | Lam [Name] Expr
  | Lit Int
  | Prim PrimOp
  deriving (Eq, Show)

data PrimOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Eq, Show)

data Defn = Defn Name Expr
  deriving (Eq, Show)

name :: Parser Name
name = Name . T.pack <$> many1 letter

num :: Parser Expr
num = Lit <$> signed decimal

var :: Parser Expr
var = Var <$> name

lam :: Parser Expr
lam = do
  string "\\"
  vars <- many1 (skipSpace *> name)
  skipSpace *> string "->"
  body <- expr
  return (Lam vars body)

eparen :: Parser Expr
eparen = char '(' *> expr <* skipSpace <* char ')'

prim :: Parser Expr
prim = Prim <$> (
      char '+' *> return Add
  <|> char '-' *> return Sub
  <|> char '*' *> return Mul
  <|> char '/' *> return Div)

expr :: Parser Expr
expr = foldl1' App <$> many1 (skipSpace *> atom)

atom :: Parser Expr
atom = try lam
    <|> eparen
    <|> prim
    <|> var
    <|> num

def :: Parser Defn
def = do
  skipSpace
  nm <- name
  skipSpace *> char '=' *> skipSpace
  ex <- expr
  skipSpace <* char ';'
  return $ Defn nm ex

file :: T.Text -> Either String [Defn]
file = parseOnly (many def <* skipSpace)

parseFile :: FilePath -> IO (Either T.Text [Defn])
parseFile path = do
  contents <- T.readFile path
  case file contents of
    Left a -> return $ Left (T.pack a)
    Right b -> return $ Right b

main :: IO (Either T.Text [Defn])
main = parseFile "simple.ml"
