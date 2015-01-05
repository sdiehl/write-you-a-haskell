module Main where

import Eval
import Type
import Check
import Parser
import Pretty
import Syntax

import Data.Maybe

import Control.Monad.Trans
import System.Console.Haskeline

eval' :: Expr -> Expr
eval' = fromJust . eval

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      let chk = check ex
      case chk of
        Left err -> print err
        Right ty -> putStrLn $ (ppexpr $ eval' ex) ++ " : " ++ (pptype ty)

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Arith> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
