module Days.Day18 where 

import Data.List.Split
import qualified Data.Set as Set
import Data.Set (Set)

import Grid3 
import Utils

parse :: String -> Position
parse s = let [x, y, z] = map read $ splitOn "," s in (x, y, z)

surface :: Set Position -> Position -> Int
surface set pos = length $ Set.difference (Set.fromList [move pos dir | dir <- allDirs]) set

floodFill :: Set Position -> Position -> Set Position
floodFill set pos = if Set.member pos set then set else
    let nexts = Set.filter inbounds $ Set.difference (Set.fromList [move pos dir | dir <- allDirs]) set
        added = Set.insert pos set
    in foldl floodFill added nexts
        where inbounds (x, y, z) = x >= 0 && y >= 0 && z >= 0 && x < 25 && y < 25 && z < 25

run :: String -> (String, String)
run s = let cubes  = map parse $ lines s
            set = Set.fromList cubes
            ans1   = sum $ map (surface set) cubes
            filled = floodFill (Set.fromList cubes) (0, 0, 0)
            inside = Set.difference (Set.fromList [(x, y, z) | x <- [0..24], y <- [0..24], z <- [0..24]]) filled
            joined = Set.union set inside
            ans2   = sum $ map (surface joined) (Set.toList joined)
        in both show (ans1, ans2)