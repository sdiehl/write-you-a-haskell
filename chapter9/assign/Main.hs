import Eval (eval)
import Parser (parseExpr)
import System.Environment

process :: String -> IO ()
process input = do
  let ast = parseExpr input
  case ast of
    Right ast -> eval ast
    Left err -> do
      putStrLn "Parser Error:"
      print err

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> putStrLn "Usage: assign <input file>"
    [fname] -> do
      contents <- readFile fname
      process contents
