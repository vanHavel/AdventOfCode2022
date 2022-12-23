module Days.Day23 where

import qualified Data.Set as Set 
import Data.Set (Set)

import Grid 

parse :: String -> Set Position 
parse s = Set.fromList [(y, x) | (y, line) <- zip [1..] $ lines s, (x, c) <- zip [1..] line, c == '#']

consider :: [(Direction, [[Direction]])]
consider = [
    (U, [[U, L], [U], [U, R]]),
    (D, [[D, L], [D], [D, R]]),
    (L, [[L, D], [L], [L, U]]),
    (R, [[R, D], [R], [R, U]])
    ]

step :: Int -> Set Position -> Set Position 
step ci cur = 
    let targets = Set.map (\p -> (p, plan ci cur p)) cur 
        duplicates = Set.fromList [pt | let ts = Set.toList targets, (p, pt) <- ts, (q, qt) <- ts, p /= q, pt == qt]
        updated = Set.map (\(p, q) -> if Set.member q duplicates then (p, p) else (p, q)) targets
    in Set.map snd updated
    

plan :: Int -> Set Position -> Position -> Position
plan ci cur p = 
    let cs = drop ci consider ++ take ci consider
        options = [dir | (dir, checks) <- cs, all (\check -> not $ Set.member (foldl move p check) cur) checks]
    in if length options == 0 || length options == 4 then p else move p (head options)
        
region :: Set Position -> Int 
region set = 
    let minx = minimum $ Set.map snd set 
        miny = minimum $ Set.map fst set
        maxx = maximum $ Set.map snd set 
        maxy = maximum $ Set.map fst set 
    in (maxx - minx + 1) * (maxy - miny + 1)

run :: String -> (String, String)
run s = 
    let start  = parse s 
        states = scanl (flip step) start (cycle [0..3])
        ans1   = region (states !! 10) - length start
        ans2   = fst $ head $ filter (\(i, ss) -> fst ss == snd ss) $ zip [1..] $ zip states $ tail states
    in (show ans1, show ans2)