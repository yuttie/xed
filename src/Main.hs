module Main (main) where

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline


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

mainLoop :: (Maybe FilePath, [Modifier String]) -> InputT IO ()
mainLoop s@(fp, ms) = do
  l <- getInputLine "> "
  case l of
    Nothing -> return ()
    Just line
      | cmd == "quit" -> return ()
      | cmd == "show" -> do
        liftIO $ putStrLn $ "file: " ++ show fp
        liftIO $ putStrLn $ "history: " ++ show ms
        mainLoop s
      | cmd == "file" -> do
        let fp' = if null args
                    then Nothing
                    else Just $ head args
        mainLoop (fp', ms)
      | cmd == "run" -> do
        case runPipeline s of
          Left  e  -> liftIO $ putStrLn e
          Right ls -> liftIO $ putStr . unlines =<< ls
        mainLoop s
      | cmd == "take" -> do
        let n = read $ head args :: Int
        let ms' = ms ++ [Modifier ("take " ++ show n) (take n)]
        mainLoop (fp, ms')
      | cmd == "drop" -> do
        let n = read $ head args :: Int
        let ms' = ms ++ [Modifier ("drop " ++ show n) (drop n)]
        mainLoop (fp, ms')
      | otherwise -> do
        liftIO $ putStrLn $ "Unknown command: " ++ cmd
        mainLoop s
      where
        (cmd, args) = parseCommandline line

main :: IO ()
main = runInputT defaultSettings $ mainLoop (Nothing, [])
