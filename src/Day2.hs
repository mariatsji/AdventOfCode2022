module Day2(day2) where

import Relude
import Data.Attoparsec.Text

data Hand = Rock | Paper | Scissors
    deriving (Eq)

data Round = Round Hand Hand

day2 :: Text -> Int
day2 txt = case parseOnly roundsParser txt of
    Left _ -> 0
    Right rounds -> sum $ fmap roundScoreTotal rounds

handScore :: Round -> Int
handScore (Round _ your )= case your of
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

roundsParser :: Parser [Round]
roundsParser = many roundParser

roundParser :: Parser Round
roundParser = do
    opp <- handParser
    _ <- space
    you <- handParser
    _ <- endOfLine <|> endOfInput
    pure $ Round opp you

handParser :: Parser Hand
handParser = rockParser <|> paperParser <|> scissorsParser
    where rockParser = Rock <$ (char 'A'  <|> char 'X')
          paperParser = Paper <$ (char 'B' <|> char 'Y')
          scissorsParser = Scissors <$ (char 'C' <|> char 'Z')