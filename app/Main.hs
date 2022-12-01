module Main where

import Relude
import Lib (day1a, day1b)

main :: IO ()
main = do
    txt1a :: Text <- decodeUtf8 <$> readFileBS "day1a.txt"
    txt1b :: Text <- decodeUtf8 <$> readFileBS "day1b.txt"
    print $ day1a txt1a
    print $ day1b txt1b
