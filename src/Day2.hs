module Day2 (solution) where

import Data.Attoparsec.Text
import Part
import Relude

data Hand = Rock | Paper | Scissors
    deriving (Eq)

data Round = Round Hand Hand

solution :: Part -> Text -> Either String Int
solution part txt =
    let parser = roundParser part
     in parseOnly (many parser) txt <&> (sum . fmap roundScoreTotal)

handScore :: Round -> Int
handScore (Round _ your) = case your of
    Rock -> 1
    Paper -> 2
    Scissors -> 3

roundScoreTotal :: Round -> Int
roundScoreTotal = liftA2 (+) roundScore handScore

-- opponent -> you
roundScore :: Round -> Int
roundScore (Round Rock Paper) = 6
roundScore (Round Paper Scissors) = 6
roundScore (Round Scissors Rock) = 6
roundScore (Round a b)
    | a == b = 3
    | otherwise = 0

roundParser :: Part -> Parser Round
roundParser part = do
    opp <- handParserA
    _ <- space
    you <- case part of
        PartA -> handParserA
        PartB -> handParserB opp
    _ <- endOfLine <|> endOfInput
    pure $ Round opp you

handParserA :: Parser Hand
handParserA = rock <|> paper <|> scissors
  where
    rock = Rock <$ (char 'A' <|> char 'X')
    paper = Paper <$ (char 'B' <|> char 'Y')
    scissors = Scissors <$ (char 'C' <|> char 'Z')

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