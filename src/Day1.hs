module Day1 (solution) where

import Data.Attoparsec.Text
import qualified Data.List
import Data.Scientific (toBoundedInteger)
import Part
import Relude

type Chunk = [Int]

solution :: Part -> Text -> Either String Int
solution part txt =
    let f = case part of
            PartA -> Data.List.maximum . fmap sum
            PartB -> sum . Data.List.take 3 . reverse . sort . fmap sum
     in parseOnly (many chunkParser) txt <&> f

chunkParser :: Parser Chunk
chunkParser = do
    nrs <- many $ do
        nr <- scientific
        _ <- endOfLine
        pure nr
    _ <- endOfLine
    pure $ fmap (fromMaybe 0 . toBoundedInteger) nrs
