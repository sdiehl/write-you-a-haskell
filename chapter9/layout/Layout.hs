{-# LANGUAGE FlexibleContexts #-}

module Layout (
  -- * Layout combinators
  IParsec,
  laidout,
  indented,
  align,
  runIndentParser,
) where

import Data.Text.Lazy

import Text.Parsec (ParseError)
import Text.Parsec.Pos
import Text.Parsec.Prim hiding (State)

import Control.Monad.Identity
import Control.Applicative ((<$>))

-- Indentation sensitive Parsec monad.
type IParsec a = Parsec Text ParseState a

data ParseState = ParseState
  { indents :: Column
  } deriving (Show)

initParseState :: ParseState
initParseState = ParseState 0

indentCmp
  :: (Column -> Column -> Bool)
  -> IParsec ()
indentCmp cmp = do
  col <- sourceColumn <$> getPosition
  current <- indents <$> getState
  guard (col `cmp` current)

withIndent :: Monad m =>Column-> Column -> ParsecT s ParseState m b -> ParsecT s ParseState m b
withIndent cur pos m = do
  modifyState $ \st -> st { indents = pos }
  res <- m
  modifyState $ \st -> st { indents = cur }
  return res

laidout :: Parsec s ParseState a -> Parsec s ParseState a
laidout m = do
  cur <- indents <$> getState
  pos <- sourceColumn <$> getPosition
  res <- withIndent cur pos m
  return res

indented :: IParsec ()
indented = indentCmp (>) <?> "Block (indented)"

align :: IParsec ()
align = indentCmp (==) <?> "Block (same indentation)"

runIndentParser
  :: Stream Text Identity a
  => SourceName
  -> IParsec a
  -> Text
  -> Either ParseError a
runIndentParser filePath p = runParser p initParseState filePath
