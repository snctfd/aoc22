module Main where

import Prelude hiding (lookup)
import RockPaperScissors
import System.IO
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromJust)

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

-- represents what outcome we want the opponent to have
theirOutcome :: String -> Outcome
theirOutcome "X" = Win
theirOutcome "Y" = Draw
theirOutcome "Z" = Lose
theirOutcome _   = error "invalid input"

-- turns a string like "A Y" into a tuple representing (my choice, their choice)
getPlay :: String -> (RPS, RPS)
getPlay s = (myPlay $ sWords !! 1, theirPlay $ sWords !! 0)
  where sWords = words s

getWinScore :: RPS -> RPS -> Maybe Integer
getWinScore myMove theirMove = Map.lookup theirMove (winScoreMap myMove)

invMap :: (Ord k, Ord v) => Map.Map k v -> Map.Map v k
invMap = Map.foldrWithKey (flip Map.insert) Map.empty

getPlay2 :: String -> (RPS, RPS)
getPlay2 s = (mine, theirs)
  where
    sWords = words s 
    theirs = theirPlay $ sWords !! 0
    mine = fromJust $ Map.lookup (theirOutcome $ sWords !! 1) (invMap $ outcomeMap theirs)
    -- since we use the opponent's move to determine which outcomeMap to use,
    -- we must also use the desired outcome *for the opponent* as the key


main :: IO ()
main = do
  fileStr <- readFile "input.txt"
  let fileLines = lines fileStr
  let plays = map getPlay fileLines
  let winScores = mapMaybe (uncurry getWinScore) plays
  let choiceScores = map (choiceScore . fst) plays

  print $ sum winScores + sum choiceScores

  let plays2 = map getPlay2 fileLines
  let winScores2 = mapMaybe (uncurry getWinScore) plays2
  let choiceScores2 = map (choiceScore . fst) plays2

  print $ sum winScores2 + sum choiceScores2
  
