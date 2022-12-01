module Lib (day1a, day1b) where

import Data.Attoparsec.Text
import qualified Data.List
import Data.Scientific (toBoundedInteger)
import Relude

type Chunk = [Int]
type Chunks = [Chunk]

day1b :: Text -> Either String Int
day1b txt =
    parseOnly chunksParser txt
        <&> (sum . Data.List.take 3 . reverse . sort . fmap sum)

day1a :: Text -> Either String Int
day1a txt =
    parseOnly chunksParser txt
        <&> (Data.List.maximum . fmap sum)

chunksParser :: Parser Chunks
chunksParser = many chunkParser

chunkParser :: Parser Chunk
chunkParser = do
    nrs <- many $ do
        nr <- scientific
        _ <- endOfLine
        pure nr
    _ <- endOfLine
    pure $ fmap (fromMaybe 0 . toBoundedInteger) nrs
