module Main where

import Relude

-- import Day1 (solution)
import Day2 (solution)
import Part

main :: IO ()
main = do
    txt <- input "day2.txt"
    print $ solution PartB txt

input :: FilePath -> IO Text
input fp = decodeUtf8 <$> readFileBS fp