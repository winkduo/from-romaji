module Main where

import           Control.Monad      (foldM_)
import           Data.Bifunctor     (bimap)
import           Data.List          (intersperse)
import           Data.Random        (StdRandom (..), runRVar)
import           Data.Random.Extras (sample)
import           Safe               (atMay)
import           System.Random      (newStdGen, randomRs)
import           Tables             (hiraganaTable, baseNumbers)
import           Text.Read          (readMaybe)


(<?>) :: Maybe a -> String -> IO a
Nothing <?> err = fail err
Just a <?> _ = pure a

infixr 5 <?>

sample' :: [(String, String)] -> IO (String, String)
sample' xs = bimap add_dots add_dots . unzip <$> runRVar (sample 3 xs) StdRandom
  where
    add_dots = mconcat . intersperse "."

help :: IO ()
help = do
  foldM_ display_col 0 hiraganaTable
  putStrLn ""
  putStrLn "Call `showCols` with a list of column indexes to see romaji"
  where
    display_col :: Int -> [(String, String)] -> IO Int
    display_col i col = do
      putStrLn $ show i <> ": " <> mconcat (intersperse ", " (map (\(h, r) -> h <> "(" <> r <> ")") col))
      pure (i + 1)

showCols :: [Int] -> IO ()
showCols cols = do
  subhiragana <- mconcat <$> traverse (hiraganaTable `atMay`) cols <?> "Out of bound column index"
  (hiragana, romaji) <- sample' subhiragana
  putStrLn romaji
  _ <- getLine
  putStrLn hiragana
  putStrLn "---------------"
  showCols cols

randomNums :: Int -> IO String
randomNums x = do
  g <- newStdGen
  return $ take x (randomRs ('1', '9') g)

getAnswer :: String -> Maybe String
getAnswer x = do
  y <- readMaybe x
  Prelude.lookup y baseNumbers

numberWang :: IO ()
numberWang = do
  num <- randomNums 1
  print num
  attempt <- getLine
  let actual = getAnswer num
  if Just attempt == actual then
    putStrLn "That's NUMBERWANG!"
  else
    putStrLn "Aah, would that it were NumberWang. Alas, it is not."
  numberWang

main :: IO ()
main = help
