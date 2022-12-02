module Days.Day2 where

import Utils

data Action = Rock | Paper | Scissors deriving (Eq, Show)
data Outcome = Win | Lose | Draw deriving (Eq, Show)

parseAction :: Char -> Action
parseAction 'A' = Rock; parseAction 'B' = Paper; parseAction 'C' = Scissors 
parseAction 'X' = Rock; parseAction 'Y' = Paper; parseAction 'Z' = Scissors

parseOutcome :: Char -> Outcome 
parseOutcome 'X' = Lose; parseOutcome 'Y' = Draw; parseOutcome 'Z' = Win

dominates :: Action -> Action 
dominates Rock     = Paper 
dominates Paper    = Scissors 
dominates Scissors = Rock

score :: Action -> Action -> Int 
score other mine = playScore mine + gameScore other mine 
  where playScore Rock        = 1
        playScore Paper       = 2 
        playScore Scissors    = 3 
        gameScore x y | x == y           = 3
                      | dominates x == y = 6
                      | otherwise        = 0

chooseAction :: Outcome -> Action -> Action 
chooseAction Win  = dominates
chooseAction Lose = twice dominates
chooseAction Draw = id

run :: String -> (String, String)
run s = 
    let rounds1 = [both parseAction (l!!0, l!!2) | l <- lines s]
        total1  = sum $ map (uncurry score) rounds1
        rounds2 = [(parseAction $ l!!0, parseOutcome $ l!!2) | l <- lines s]
        total2 = sum $ map (\(a, o) -> score a (chooseAction o a)) rounds2 
    in both show (total1, total2)