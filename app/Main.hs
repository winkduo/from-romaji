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

searchNum :: Int -> Maybe String
searchNum x = Prelude.lookup x baseNumbers

randomNums :: Int -> IO String
randomNums x = do
  g <- newStdGen
  return $ take x (randomRs ('1', '9') g)

getAnswer :: String -> Maybe String
getAnswer x = do
  y <- readMaybe x
  countInJapanese y

numberWang :: IO ()
numberWang = do
  g <- newStdGen
  let h = take 1 (randomRs ('1', '5') g)
  num <- randomNums (read h)
  print num
  attempt <- getLine
  let actual = getAnswer num
  if Just attempt == actual then
    putStrLn "That's NUMBERWANG!"
  else
    putStrLn "Aah, would that it were NumberWang. Alas, it is not."
  numberWang

toRevDigits :: Int -> [Int]
toRevDigits n
  | n <= 0    = []
  | otherwise =  d : toRevDigits ds
     where
       d  = lastDigit n
       ds = dropLastDigit n
       lastDigit     = (`mod` 10)
       dropLastDigit = (`div` 10)

countInJapanese :: Int -> Maybe String
countInJapanese x =
  if searchNum x == Nothing
    then reverse . drop 5 . reverse <$> genNumbers x
  else searchNum x 

genNumbers :: Int -> Maybe String
genNumbers x = foldl look Nothing $ zip [0..] (toRevDigits x)
    where
      look :: Maybe String -> (Integer, Int) -> Maybe String
      look acc (1, 1) = searchNum 10 <> Just " " <> acc      
      look acc (2, 1) = searchNum 100 <> Just " " <> acc
      look acc (3, 1) = searchNum 1000 <> Just " " <> acc

      look acc (3, 0) = searchNum 0 <> Just " " <> acc
      look acc (2, 0) = searchNum 0 <> Just " " <> acc
      look acc (1, 0) = searchNum 0 <> Just " " <> acc

      look acc (i, v) = searchNum v <> searchNum (10 ^ i) <> Just " " <> acc

digitValues :: Int -> [Int]
digitValues x = foldl arith [] $ zip [0..] (toRevDigits x)
    where
      arith :: [Int] -> (Int, Int) -> [Int]
      arith acc (i, v) = acc ++ [v * 10 ^ i]

-- If you have a table you want to use other than baseNumbers
countInJapanese' :: Int -> [(Int, String)] -> String
countInJapanese' x table = foldl readNumbers "" (reverse $ digitValues x)
    where
      readNumbers acc y = acc ++ " " ++ show (Prelude.lookup y table)

main :: IO ()
main = help
