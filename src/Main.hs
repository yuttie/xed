{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
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

class StreamType a where
  toString   :: Stream a -> [Char]
  fromString :: [Char]   -> Stream a

instance StreamType Char where
  toString (Stream xs) = xs
  fromString xs        = Stream xs

instance StreamType String where
  toString (Stream xs) = unlines xs
  fromString xs        = Stream $ lines xs

data StreamType u => Stream u = Stream [u]

mapStream :: StreamType u => ([u] -> [u]) -> Stream u -> Stream u
mapStream f (Stream xs) = Stream $ f xs

data StreamType u => Modifier u = Modifier String (Stream u -> Stream u)

instance StreamType u => Show (Modifier u) where
  show (Modifier name _) = name

encapsulate :: (StreamType u, StreamType v)
            => String -> [Modifier v] -> Modifier u
encapsulate name ms = Modifier (name ++ "(" ++ show ms ++ ")")
                               (\s -> convert $ applyModifiers (convert s) ms)
  where
    convert :: (StreamType a, StreamType b) => Stream a -> Stream b
    convert = fromString . toString

applyModifiers :: StreamType u => Stream u -> [Modifier u] -> Stream u
applyModifiers s []                  = s
applyModifiers s (Modifier _ f : ms) = applyModifiers (f s) ms

runPipeline :: (Maybe FilePath, [Modifier String])
            -> Either String (IO (Stream String))
runPipeline (Nothing, _)  = Left "Error: source file is not specified."
runPipeline (Just fp, ms) = Right $ do
  ls <- liftM lines $ readFile fp
  return $ applyModifiers (Stream ls) ms

nestLoop :: StreamType u => String -> [Modifier u] -> InputT IO [Modifier u]
nestLoop name ms = do
  l <- getInputLine $ name ++ "> "
  case l of
    Nothing -> return ms
    Just line
      | cmd == "quit" -> return ms
      | cmd == "show" -> do
          liftIO $ putStrLn $ "pipeline: " ++ show ms
          nestLoop name ms
      | cmd == "take" -> do
          let n = read $ head args :: Int
          let ms' = ms ++ [Modifier ("take " ++ show n) (mapStream $ take n)]
          nestLoop name ms'
      | cmd == "drop" -> do
          let n = read $ head args :: Int
          let ms' = ms ++ [Modifier ("drop " ++ show n) (mapStream $ drop n)]
          nestLoop name ms'
      | otherwise -> do
          liftIO $ putStrLn $ "Unknown command: " ++ cmd
          nestLoop name ms
      where
        (cmd, args) = parseCommandline line

mainLoop :: (Maybe FilePath, [Modifier String]) -> InputT IO ()
mainLoop s@(fp, ms) = do
  l <- getInputLine "> "
  case l of
    Nothing -> return ()
    Just line
      | cmd == "quit" -> return ()
      | cmd == "show" -> do
          liftIO $ putStrLn $ "file: " ++ show fp
          liftIO $ putStrLn $ "pipeline: " ++ show ms
          mainLoop s
      | cmd == "file" -> do
          let fp' = if null args
                      then Nothing
                      else Just $ head args
          mainLoop (fp', ms)
      | cmd == "run" -> do
          case runPipeline s of
            Left  e   -> liftIO $ putStrLn e
            Right mls -> liftIO $ do
              Stream ls <- mls
              putStr $ unlines ls
          mainLoop s
      | cmd == "chars" -> do
          innerPipeline <- nestLoop "chars" [] :: InputT IO [Modifier Char]
          let ms' = ms ++ [encapsulate "chars" innerPipeline]
          if null innerPipeline
            then mainLoop s
            else mainLoop (fp, ms')
      | cmd == "take" -> do
          let n = read $ head args :: Int
          let ms' = ms ++ [Modifier ("take " ++ show n) (mapStream $ take n)]
          mainLoop (fp, ms')
      | cmd == "drop" -> do
          let n = read $ head args :: Int
          let ms' = ms ++ [Modifier ("drop " ++ show n) (mapStream $ drop n)]
          mainLoop (fp, ms')
      | otherwise -> do
          liftIO $ putStrLn $ "Unknown command: " ++ cmd
          mainLoop s
      where
        (cmd, args) = parseCommandline line

main :: IO ()
main = runInputT defaultSettings $ mainLoop (Nothing, [])
