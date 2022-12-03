module Day2 (solution) where

import Data.Attoparsec.Text
import Part
import Relude

data Hand = Rock | Paper | Scissors
    deriving (Eq)

data Round = Round Hand Hand

solution :: Part -> Text -> Either String Int
solution part txt =
    let parser = case part of
            PartA -> roundParserA
            PartB -> roundParserB
     in parseOnly (many parser) txt <&> (sum . fmap roundScoreTotal)

handScore :: Round -> Int
handScore (Round _ your) = case your of
    Rock -> 1
    Paper -> 2
    Scissors -> 3

roundScoreTotal :: Round -> Int
roundScoreTotal r = roundScore r + handScore r

-- opponent -> you
roundScore :: Round -> Int
roundScore (Round Rock Paper) = 6
roundScore (Round Paper Scissors) = 6
roundScore (Round Scissors Rock) = 6
roundScore (Round a b)
    | a == b = 3
    | otherwise = 0

roundParserB :: Parser Round
roundParserB = do
    opp <- handParserA
    _ <- space
    you <- handParserB opp
    _ <- endOfLine <|> endOfInput
    pure $ Round opp you

roundParserA :: Parser Round
roundParserA = do
    opp <- handParserA
    _ <- space
    you <- handParserA
    _ <- endOfLine <|> endOfInput
    pure $ Round opp you

handParserA :: Parser Hand
handParserA = rockParser <|> paperParser <|> scissorsParser
  where
    rockParser = Rock <$ (char 'A' <|> char 'X')
    paperParser = Paper <$ (char 'B' <|> char 'Y')
    scissorsParser = Scissors <$ (char 'C' <|> char 'Z')

handParserB :: Hand -> Parser Hand
handParserB opp =
    looseTo opp <$ char 'X'
        <|> drawTo opp <$ char 'Y'
        <|> winTo opp <$ char 'Z'
  where
    looseTo Paper = Rock
    looseTo Rock = Scissors
    looseTo Scissors = Paper
    drawTo x = x
    winTo Paper = Scissors
    winTo Rock = Paper
    winTo Scissors = Rock