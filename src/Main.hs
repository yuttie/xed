module Main (main) where

import System.Environment
import System.Console.Editline


main :: IO ()
main = do
  progName <- getProgName
  el <- elInit progName
  setPrompt el (return "> ")
  setEditor el Vi

  l <- elGets el
  case l of
    Nothing -> return ()
    Just line -> do
      putStrLn $ "cmd: " ++ show (init line)
