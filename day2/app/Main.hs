module Main where

import Prelude hiding (lookup)
import RockPaperScissors
import System.IO
import Data.Map hiding (map, mapMaybe)
import Data.Maybe (mapMaybe)

theirPlay :: String -> RPS
theirPlay "A" = Rock
theirPlay "B" = Paper
theirPlay "C" = Scissors
theirPlay _   = error "invalid input"

myPlay :: String -> RPS
myPlay "X" = Rock
myPlay "Y" = Paper
myPlay "Z" = Scissors
myPlay _   = error "invalid input"

getPlay :: String -> (RPS, RPS)
getPlay s = (myPlay $ sWords !! 1, theirPlay $ sWords !! 0)
  where sWords = words s

getWinScore :: RPS -> RPS -> Maybe Integer
getWinScore myMove theirMove = lookup theirMove (winScoreMap myMove)

main :: IO ()
main = do
  fileStr <- readFile "input.txt"
  let fileLines = lines fileStr
  let plays = map getPlay fileLines
  let winScores = mapMaybe (uncurry getWinScore) plays
  let choiceScores = map (choiceScore . fst) plays
  print $ sum winScores + sum choiceScores
  
