module Main where

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Text.Parsec.Language (haskellStyle)

import Data.List
import Data.Function

import Control.Monad.Identity (Identity)

import Text.Parsec
import qualified Text.Parsec as P

type Name = String

data Expr
  = Var Name
  | Lam Name Expr
  | App Expr Expr
  | Let Name Expr Expr
  | BinOp Name Expr Expr
  | UnOp Name Expr
  deriving (Show)

data Assoc
  = OpLeft
  | OpRight
  | OpNone
  | OpPrefix
  | OpPostfix
  deriving Show

data Decl
  = LetDecl Expr
  | OpDecl OperatorDef
  deriving (Show)

type Op x = Ex.Operator String ParseState Identity x
type Parser a = Parsec String ParseState a
data ParseState = ParseState [OperatorDef] deriving Show

data OperatorDef = OperatorDef {
    oassoc :: Assoc
  , oprec :: Integer
  , otok :: Name
  } deriving Show

lexer :: Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser style
  where ops = ["->","\\","+","*","<","=","[","]","_"]
        names = ["let","in","infixl", "infixr", "infix", "postfix", "prefix"]
        style = haskellStyle { Tok.reservedOpNames = ops
                             , Tok.reservedNames = names
                             , Tok.identLetter = alphaNum <|> oneOf "#'_"
                             , Tok.commentLine = "--"
                             }

reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
identifier = Tok.identifier lexer
parens     = Tok.parens lexer
brackets   = Tok.brackets lexer
braces     = Tok.braces lexer
commaSep   = Tok.commaSep lexer
semi       = Tok.semi lexer
integer    = Tok.integer lexer
chr        = Tok.charLiteral lexer
str        = Tok.stringLiteral lexer
operator   = Tok.operator lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- identifier
  reservedOp "->"
  body <- expr
  return $ Lam args body

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (Let x e1 e2)

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)


addOperator :: OperatorDef -> Parser ()
addOperator a = P.modifyState $ \(ParseState ops) -> ParseState (a : ops)

mkTable :: ParseState -> [[Op Expr]]
mkTable (ParseState ops) =
  map (map toParser) $
    groupBy ((==) `on` oprec) $
      reverse $ sortBy (compare `on` oprec) $ ops

toParser :: OperatorDef -> Op Expr
toParser (OperatorDef ass _ tok) = case ass of
    OpLeft    -> infixOp tok (BinOp tok) (toAssoc ass)
    OpRight   -> infixOp tok (BinOp tok) (toAssoc ass)
    OpNone    -> infixOp tok (BinOp tok) (toAssoc ass)
    OpPrefix  -> prefixOp tok (UnOp tok)
    OpPostfix -> postfixOp tok (UnOp tok)
  where
    toAssoc OpLeft = Ex.AssocLeft
    toAssoc OpRight = Ex.AssocRight
    toAssoc OpNone = Ex.AssocNone
    toAssoc _ = error "no associativity"

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

prefixOp :: String -> (a -> a) -> Ex.Operator String u Identity a
prefixOp name f = Ex.Prefix (reservedOp name >> return f)

postfixOp :: String -> (a -> a) -> Ex.Operator String u Identity a
postfixOp name f = Ex.Postfix (reservedOp name >> return f)

term :: Parser Expr
term = do
  tbl <- getState
  let table = mkTable tbl
  Ex.buildExpressionParser table aexp

aexp :: Parser Expr
aexp =  letin
    <|> lambda
    <|> variable
    <|> parens expr

letdecl :: Parser Decl
letdecl = do
  e <- expr
  return $ LetDecl e


opleft :: Parser Decl
opleft = do
  reserved "infixl"
  prec <- integer
  sym <- parens operator
  let op = (OperatorDef OpLeft prec sym)
  addOperator op
  return $ OpDecl op

opright :: Parser Decl
opright = do
  reserved "infixr"
  prec <- integer
  sym <- parens operator
  let op = (OperatorDef OpRight prec sym)
  addOperator op
  return $ OpDecl op

opnone :: Parser Decl
opnone = do
  reserved "infix"
  prec <- integer
  sym <- parens operator
  let op = (OperatorDef OpNone prec sym)
  addOperator op
  return $ OpDecl op

opprefix :: Parser Decl
opprefix = do
  reserved "prefix"
  prec <- integer
  sym <- parens operator
  let op = OperatorDef OpPrefix prec sym
  addOperator op
  return $ OpDecl op

oppostfix :: Parser Decl
oppostfix = do
  reserved "postfix"
  prec <- integer
  sym <- parens operator
  let op = OperatorDef OpPostfix prec sym
  addOperator op
  return $ OpDecl op

decl :: Parser Decl
decl =
    try letdecl
    <|> opleft
    <|> opright
    <|> opnone
    <|> opprefix
    <|> oppostfix

top :: Parser Decl
top = do
  x <- decl
  P.optional semi
  return x


modl :: Parser [Decl]
modl = many top

parseModule :: SourceName -> String -> Either ParseError [Decl]
parseModule filePath = P.runParser (contents modl) (ParseState []) filePath

main :: IO ()
main = do
  input <- readFile "test.in"
  let res = parseModule "<stdin>" input
  case res of
    Left err -> print err
    Right ast -> mapM_ print ast
