module Main (main) where

import System.Environment
import System.Console.Editline


mainLoop :: [String] -> [IO (Maybe String)] -> IO ()
mainLoop xs (a:as) = do
  l <- a
  case l of
    Nothing   -> return ()
    Just line -> do
      let xs' = xs ++ ["cmd: " ++ (init line)]
      print xs'
      mainLoop xs' as

main :: IO ()
main = do
  progName <- getProgName
  el <- elInit progName
  setPrompt el (return "> ")
  setEditor el Vi

  let as = repeat $ elGets el
  mainLoop [] as
