module Main where

import Relude
import Day1 (day1a, day1b)
import Day2 (day2)

main :: IO ()
main = mainday2

mainday2 :: IO ()
mainday2 = do
    txt <- decodeUtf8 <$> readFileBS "day2.txt"
    print $ day2 txt

mainday1 :: IO ()
mainday1 = do
    txt :: Text <- decodeUtf8 <$> readFileBS "day1.txt"
    print $ day1a txt
    print $ day1b txt
