{
module Lexer (
  Token(..),
  scanTokens
) where

import Syntax
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;
  print                         { \s -> TokenPrint }
  $digit+                       { \s -> TokenNum (read s) }
  \=                            { \s -> TokenEq }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

data Token 
  = TokenNum Int
  | TokenSym String
  | TokenPrint
  | TokenEq
  | TokenEOF
  deriving (Eq,Show)

scanTokens = alexScanTokens

}
