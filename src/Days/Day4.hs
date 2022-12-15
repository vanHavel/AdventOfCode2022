module Days.Day4 where

import Data.Maybe

import Interval
import Utils

parseInterval :: String -> Interval
parseInterval = splittingParse "-" read 

parseLine :: String -> (Interval, Interval)
parseLine = splittingParse "," parseInterval

run :: String -> (String, String)
run s = 
    let pairs = map parseLine $ lines s 
        ans1 = count (\(i1, i2) -> (i1 `intersect` i2) `elem` [Just i1, Just i2] ) pairs 
        ans2 = count (isJust . uncurry intersect) pairs
    in both show (ans1, ans2)