module RockPaperScissors where

import Data.Map

data RPS = Rock | Paper | Scissors deriving (Ord, Eq)

winScoreMap :: RPS -> Map RPS Integer
winScoreMap Rock     = fromList [(Rock, 3), (Paper, 0), (Scissors, 6)]
winScoreMap Paper    = fromList [(Rock, 6), (Paper, 3), (Scissors, 0)]
winScoreMap Scissors = fromList [(Rock, 0), (Paper, 6), (Scissors, 3)]

choiceScore :: RPS -> Integer
choiceScore Rock = 1
choiceScore Paper = 2
choiceScore Scissors = 3