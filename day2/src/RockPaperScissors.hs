module RockPaperScissors where

import Data.Map

data RPS = Rock | Paper | Scissors deriving (Ord, Eq, Show)
data Outcome = Lose | Draw | Win deriving (Ord, Eq, Show)

winScoreMap :: RPS -> Map RPS Integer
winScoreMap Rock     = fromList [(Rock, 3), (Paper, 0), (Scissors, 6)]
winScoreMap Paper    = fromList [(Rock, 6), (Paper, 3), (Scissors, 0)]
winScoreMap Scissors = fromList [(Rock, 0), (Paper, 6), (Scissors, 3)]

choiceScore :: RPS -> Integer
choiceScore Rock = 1
choiceScore Paper = 2
choiceScore Scissors = 3

outcomeMap :: RPS -> Map RPS Outcome
outcomeMap Rock     = fromList [(Rock, Draw), (Paper, Lose), (Scissors, Win)]
outcomeMap Paper    = fromList [(Rock, Win), (Paper, Draw), (Scissors, Lose)]
outcomeMap Scissors = fromList [(Rock, Lose), (Paper, Win), (Scissors, Draw)]