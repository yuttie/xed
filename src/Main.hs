module Main (main) where

import Control.Monad
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

data Modifier u = Modifier String ([u] -> [u])

instance Show (Modifier u) where
  show (Modifier n _) = n

applyModifiers :: [String] -> [Modifier String] -> [String]
applyModifiers ls []                  = ls
applyModifiers ls (Modifier _ f : ms) = applyModifiers (f ls) ms

runPipeline :: (Maybe FilePath, [Modifier String]) -> Either String (IO [String])
runPipeline (Nothing, _)  = Left "Error: source file is not specified."
runPipeline (Just fp, ms) = Right $ do
  ls <- liftM lines $ readFile fp
  return $ applyModifiers ls ms

mainLoop :: (Maybe FilePath, [Modifier String]) -> [IO (Maybe String)] -> IO ()
mainLoop _          []     = return ()
mainLoop s@(fp, ms) (a:as) = do
  l <- a
  case l of
    Nothing -> return ()
    Just line
      | cmd == "quit" -> do
        return ()
      | cmd == "show" -> do
        putStrLn $ "file: " ++ show fp
        putStrLn $ "history: " ++ show ms
        mainLoop s as
      | cmd == "file" -> do
        let fp' = if null args
                    then Nothing
                    else Just $ head args
        mainLoop (fp', ms) as
      | cmd == "run" -> do
        case runPipeline s of
          Left  e  -> putStrLn e
          Right ls -> putStr . unlines =<< ls
        mainLoop s as
      | cmd == "take" -> do
        let n = read $ head args :: Int
        let ms' = ms ++ [Modifier ("take " ++ show n) (take n)]
        mainLoop (fp, ms') as
      | cmd == "drop" -> do
        let n = read $ head args :: Int
        let ms' = ms ++ [Modifier ("drop " ++ show n) (drop n)]
        mainLoop (fp, ms') as
      | otherwise -> do
        putStrLn $ "Unknown command: " ++ cmd
        mainLoop s as
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
