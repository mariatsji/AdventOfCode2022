module Main where

import Relude
import Lib (day1a, day1b)

main :: IO ()
main = do
    txt :: Text <- decodeUtf8 <$> readFileBS "day1.txt"
    print $ day1a txt
    print $ day1b txt
