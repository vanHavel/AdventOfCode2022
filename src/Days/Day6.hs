module Days.Day6 where

import Data.List

import Utils

run :: String -> (String, String)
run s = both show (firstUnique 4 s, firstUnique 14 s)

firstUnique :: Int -> String -> Int
firstUnique = firstUnique' 0
    where firstUnique' p k s@(c:ss) | (length $ nub $ take k s) == k = p + k
                                    | otherwise                      = firstUnique' (succ p) k ss