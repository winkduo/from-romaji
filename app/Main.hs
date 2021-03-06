module Main where

import Control.Monad (foldM_)
import Data.Bifunctor (bimap)
import Data.List (intercalate, intersperse)
import Data.Maybe (fromMaybe)
import Data.Random (StdRandom(..), runRVar)
import Data.Random.Extras (sample)
import Data.Traversable
import Safe (atMay)
import System.Console.ANSI
import System.Console.Readline (readline)
import System.Random (newStdGen, randomRs)
import Tables (baseNumbers, hiraganaTable, katakanaTable)
import Text.Read (readMaybe)

(<?>) :: Maybe a -> String -> IO a
Nothing <?> err = fail err
Just a <?> _ = pure a

infixr 5 <?>

sample' :: Int -> [(String, String)] -> IO (String, [String])
sample' len xs = bimap add_dots id . unzip <$> runRVar (sample len xs) StdRandom
  where
    add_dots = mconcat -- . intersperse "."

help :: IO ()
help = do
  foldM_ display_col 0 hiraganaTable
  putStrLn ""
  putStrLn "Call `showRomaji` with a list of column indexes to see romaji"
  where
    display_col :: Int -> [(String, String)] -> IO Int
    display_col i col = do
      putStrLn $
        show i <>
        ": " <>
        mconcat (intersperse ", " (map (\(h, r) -> h <> "(" <> r <> ")") col))
      pure (i + 1)

showRomaji :: Int -> [Int] -> IO ()
showRomaji len cols = do
  subtable <-
    mconcat <$>
    traverse (hiraganaTable `atMay`) cols <?> "Out of bound column index"
  (tableChars, romajiChars) <- sample' len subtable
  for romajiChars $ \romajiChar -> print romajiChar
  putStrLn ""
  _ <- getLine
  putStrLn "---------------"
  showRomaji len cols

tableToRomaji :: [[(String, String)]] -> (Int -> [Int] -> IO ())
tableToRomaji table len cols = do
  setSGR [SetColor Foreground Vivid White]
  subtable <-
    mconcat <$>
    traverse (table `atMay`) (take (length table) cols) <?>
    "Out of bound column index"
  (tableChars, romajiChars) <- sample' len subtable
  putStrLn tableChars
  input <- fromMaybe "" <$> readline ""
  if input == mconcat romajiChars
    then do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn "Correct!"
      setSGR [SetColor Foreground Vivid White]
    else do
      putStrLn "Failed!"
      foldM_
        (\i c -> do
           if take (length c) (drop i input) == c
             then setSGR [SetColor Foreground Vivid Magenta]
             else setSGR [SetColor Foreground Vivid Red]
           putStr c
           pure $ i + length c)
        0
        romajiChars
      setSGR [SetColor Foreground Vivid White]
      putStrLn ""
  putStrLn "---------------"
  tableToRomaji table len cols

hiraganaToRomaji :: Int -> [Int] -> IO ()
hiraganaToRomaji = tableToRomaji hiraganaTable

katakanaToRomaji :: Int -> [Int] -> IO ()
katakanaToRomaji = tableToRomaji katakanaTable

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

-- `numberWang` generates a digit number (1 to 5) and
-- using that a random quiz number.
-- If the guess is equal to the actual translation THAT'S NUMBERWANG!
numberWang :: Int -> IO ()
numberWang x = do
  g <- newStdGen
  let (h:_) = randomRs (1, x) g
  if x > 5
    then putStrLn "Numbers of six and greater digits are not yet translated."
    else do
      num <- randomNums h
      print num
      attempt <- getLine
      let actual = getAnswer num
      if Just attempt == actual
        then putStrLn "That's NUMBERWANG!"
        else putStrLn "Aah, would that it were NumberWang. Alas, it is not."
      numberWang x

type DigitIndex = Integer

type Digit = Int

type Translation = String

-- Given an Int get its digits in reversed order.
-- λ> toRevDigits 12345
-- [5,4,3,2,1]
toRevDigits :: Int -> [Int]
toRevDigits n
  | n <= 0 = []
  | otherwise = d : toRevDigits ds
  where
    d = lastDigit n
    ds = dropLastDigit n
    lastDigit = (`mod` 10)
    dropLastDigit = (`div` 10)

-- Given an Int `countInJapanese` checks whether that Int has a baseNumbers entry or not.
-- If the lookup is unsuccessful then call `genNumbers` function with that number
-- to try and generate an appropriate response.
-- (reverse . drop 5 . reverse) here is a little hack to erase _ichi suffix
countInJapanese :: Int -> Maybe Translation
countInJapanese x =
  if searchNum x == Nothing
    then reverse . drop 5 . reverse <$> genNumbers x
    else searchNum x

-- Given an Int `genNumbers` tries to generate a romaji translation for that number.
-- We zip [0..] with the input list `toRevDigits x` to get a list of tuples
-- containing digits in reversed order and their indexes.
-- look function then uses these tuples and previous outputs to
-- generate a final translation.
genNumbers :: Int -> Maybe Translation
genNumbers x = foldl look Nothing $ zip [0 ..] (toRevDigits x)
  where
    look :: Maybe Translation -> (DigitIndex, Digit) -> Maybe Translation
  -- Since numbers 10, 100 and 1000 don't get number prefixes
  -- e.g 10 is juu and not ichi juu
  -- look function generates translations for them without.
    look acc (1, 1) = searchNum 10 <> Just " " <> acc
    look acc (2, 1) = searchNum 100 <> Just " " <> acc
    look acc (3, 1) = searchNum 1000 <> Just " " <> acc
  -- Regarding numbers with zeros in between
  -- e.g 30002
  -- zeros also gets translated since `searchNum (10 ^ 0)`
  -- gets evaluated as `searchNum 1`
  -- This situation is handled with special cases below.
    look acc (3, 0) = searchNum 0 <> Just " " <> acc
    look acc (2, 0) = searchNum 0 <> Just " " <> acc
    look acc (1, 0) = searchNum 0 <> Just " " <> acc
    look acc (i, v) = searchNum v <> searchNum (10 ^ i) <> Just " " <> acc

-- Given an Int get its place value, starting with the smallest.
-- λ> digitValues 12345
-- [5,40,300,2000,10000]
placeValue :: Int -> [Int]
placeValue x = foldl arith [] $ zip [0 ..] (toRevDigits x)
  where
    arith :: [Int] -> (Int, Int) -> [Int]
    arith acc (i, v) = acc ++ [v * 10 ^ i]

-- If you have a table you want to use other than baseNumbers
countInJapanese' :: Int -> [(Int, String)] -> Translation
countInJapanese' x table = foldl readNumbers "" (reverse $ placeValue x)
  where
    readNumbers acc y = acc ++ " " ++ show (Prelude.lookup y table)

main :: IO ()
main = help
