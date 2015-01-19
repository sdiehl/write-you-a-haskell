{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseExpr,
) where

import Lexer
import Syntax

import Control.Monad.Except
}

%name expr
%tokentype { Token }
%monad { Except String } { (>>=) } { return }
%error { parseError }

%token
    int   { TokenNum $$ }
    var   { TokenSym $$ }
    print { TokenPrint }
    '='   { TokenEq }

%%

terms 
    : term                   { [$1] }
    | term terms             { $1 : $2 }

term 
   : var                     { Var $1 }
   | var '=' int             { Assign $1 $3 }
   | print term              { Print $2 }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String [Expr]
parseExpr input = 
  let tokenStream = scanTokens input in
  runExcept (expr tokenStream)
}
