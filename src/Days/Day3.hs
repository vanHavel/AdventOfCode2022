module Days.Day3 where

import Data.Char 
import Data.List.Split

import Utils

letters :: [Char]
letters = ['a'..'z'] ++ ['A'..'Z']

priority :: Char -> Int 
priority c | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1 
           | otherwise            = ord c - ord 'A' + 27

run :: String -> (String, String)
run s = 
    let splitpacks = [(take k xs, drop k xs) | xs <- lines s, let k = div (length xs) 2]
        specials   = [c | (left, right) <- splitpacks, c <- letters, elem c left, elem c right]
        grouppacks = chunksOf 3 $ lines s
        badges     = [c | group <- grouppacks, c <- letters, all (elem c) group]
    in both (show . sum . map priority) (specials, badges)