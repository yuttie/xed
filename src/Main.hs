module Main (main) where

import System.Environment
import System.Console.Editline


split :: (Char -> Bool) -> String -> [String]
split _ [] = [[]]
split p xs = case ys' of
               []   -> [ys]
               _:zs -> ys : split p zs
  where
    ys  = takeWhile (not . p) xs
    ys' = dropWhile (not . p) xs
    
parseCommandline :: String -> (String, [String])
parseCommandline l = (cmd, args')
  where
    (cmd:args) = split (== ' ') l
    args' = filter (not . null) args

mainLoop :: (Maybe FilePath, [String]) -> [IO (Maybe String)] -> IO ()
mainLoop _          []     = return ()
mainLoop s@(fp, xs) (a:as) = do
  l <- a
  case l of
    Nothing -> return ()
    Just line
      | cmd == "quit" -> do
        return ()
      | cmd == "show" -> do
        putStrLn $ "file: " ++ show fp
        putStrLn $ "history: " ++ show xs
        mainLoop s as
      | cmd == "file" -> do
        let fp' = if null args
                    then Nothing
                    else Just $ head args
        mainLoop (fp', xs) as
      | otherwise -> do
        let xs' = xs ++ ["cmd: " ++ cmd]
        mainLoop (fp, xs') as
      where
        (cmd, args) = parseCommandline $ init line

main :: IO ()
main = do
  progName <- getProgName
  el <- elInit progName
  setPrompt el (return "> ")
  setEditor el Vi

  let as = repeat $ elGets el
  mainLoop (Nothing, []) as
