module Main (main) where

import System.Environment
import System.Console.Editline


mainLoop :: [String] -> [IO (Maybe String)] -> IO ()
mainLoop _  []     = return ()
mainLoop xs (a:as) = do
  l <- a
  case l of
    Nothing -> return ()
    Just line
      | line' == "quit" -> do
        return ()
      | line' == ":show" -> do
        print xs
        mainLoop xs as
      | otherwise -> do
        let xs' = xs ++ ["cmd: " ++ line']
        mainLoop xs' as
      where
        line' = init line

main :: IO ()
main = do
  progName <- getProgName
  el <- elInit progName
  setPrompt el (return "> ")
  setEditor el Vi

  let as = repeat $ elGets el
  mainLoop [] as
