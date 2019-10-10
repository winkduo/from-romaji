module Main where

import           Control.Monad      (foldM_)
import           Data.Bifunctor     (bimap)
import           Data.List          (intersperse)
import           Data.Random        (StdRandom (..), runRVar)
import           Data.Random.Extras (sample)
import           Safe               (atMay)

hiraganaTable :: [[(String, String)]]
hiraganaTable =
  [ [ ("あ", "a")
    , ("い", "i")
    , ("う", "u")
    , ("え", "e")
    , ("お", "o")
    ]
  , [ ("か", "ka")
    , ("き", "ki")
    , ("く", "ku")
    , ("け", "ke")
    , ("こ", "ko")
    ]
  , [ ("が", "ga")
    , ("ぎ", "gi")
    , ("ぐ", "gu")
    , ("げ", "ge")
    , ("ご", "go")
    ]
  , [ ("さ", "sa")
    , ("し", "shi")
    , ("す", "su")
    , ("せ", "se")
    , ("そ", "so")
    ]
  , [ ("ざ", "za")
    , ("じ", "ji")
    , ("ず", "zu")
    , ("ぜ", "ze")
    , ("ぞ", "zo")
    ]
  , [ ("た", "ta")
    , ("ち", "chi")
    , ("つ", "tsu")
    , ("て", "te")
    , ("と", "to")
    ]
  , [ ("だ", "da")
    , ("ぢ", "ji")
    , ("づ", "zu")
    , ("で", "de")
    , ("ど", "do")
    ]
  , [ ("な", "na")
    , ("に", "ni")
    , ("ぬ", "nu")
    , ("ね", "ne")
    , ("の", "no")
    ]
  , [ ("は", "ha")
    , ("ひ", "hi")
    , ("ふ", "fu")
    , ("へ", "he")
    , ("ほ", "ho")
    ]
  , [ ("ば", "ba")
    , ("び", "bi")
    , ("ぶ", "bu")
    , ("べ", "be")
    , ("ぼ", "bo")
    ]
  , [ ("ぱ", "pa")
    , ("ぴ", "pi")
    , ("ぷ", "pu")
    , ("ぺ", "pe")
    , ("ぽ", "po")
    ]
  , [ ("ま", "ma")
    , ("み", "mi")
    , ("む", "mu")
    , ("め", "me")
    , ("も", "mo")
    ]
  , [ ("や", "ya")
    , ("ゆ", "yu")
    , ("よ", "yo")
    , ("ら", "ra")
    , ("り", "ri")
    ]
  , [ ("る", "ru")
    , ("れ", "re")
    , ("ろ", "ro")
    , ("わ", "wa")
    , ("を", "wo")
    ]
  , [ ("ん", "n/m")
    ]
  ]

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

main :: IO ()
main = help
