import Syntax (Expr)
import Eval (runEval)
import Parser (parseExpr, parseTokens)

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process input = do
  let tokens = parseTokens input
  putStrLn ("Tokens: " ++ show tokens)
  let ast = parseExpr input
  putStrLn ("Syntax: " ++ show ast)
  case ast of
    Left err -> do
      putStrLn "Parse Error:"
      print err
    Right ast -> exec ast

exec :: Expr -> IO ()
exec ast = do
  let result = runEval ast
  case result of
    Left err -> do
      putStrLn "Runtime Error:"
      putStrLn err
    Right res -> print res

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Happy> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
