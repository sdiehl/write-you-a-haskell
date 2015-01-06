{-# LANGUAGE OverloadedStrings #-}

import Text.Read
import Control.Monad.State

import Text.Pandoc

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

doSlice :: Block -> IO Block
doSlice cb@(CodeBlock (id, classes, namevals) contents) = do
  res <- return $ do
    upper <- readMaybe =<< lookup "upper" namevals
    lower <- readMaybe =<< lookup "lower" namevals
    file  <- lookup "slice" namevals
    return (upper, lower, file)

  case res of
    Nothing -> return cb
    Just (upper, lower, f) -> do
      contents <- readFile f
      let lns = unlines $ slice lower upper (lines contents)
      return (CodeBlock (id, classes, namevals) lns)
doSlice x = return x

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> return . (CodeBlock (id, classes, namevals)) =<< readFile f
       Nothing    -> return cb
doInclude x = return x

doHtml :: Block -> IO Block
doHtml cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "literal" namevals of
       Just f     -> return . (RawBlock "html") =<< readFile f
       Nothing    -> return cb
doHtml x = return x

main :: IO ()
main = getContents >>= return . readMarkdown def
                   >>= bottomUpM doInclude
                   >>= bottomUpM doSlice
                   >>= bottomUpM doHtml
                   >>= putStrLn . writeMarkdown def
