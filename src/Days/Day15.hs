module Days.Day15 where 

import Data.Char 
import Data.Maybe
import Data.List (sort, nub)

import Grid 
import Interval
import Utils


join :: [Interval] -> [Interval]
join is = if sort (joinStep is) == sort is then is else join $ joinStep is

joinStep :: [Interval] -> [Interval]
joinStep [] = []
joinStep (i:is) = let (j:js) = join' i is in j:(joinStep js)
  where join' i [] = [i]
        join' i (j:js) = if isJust (intersect i j) then join' (union i j) js else (join' i js) ++ [j]

blocks :: Int -> Position -> Position -> Maybe Interval 
blocks ly sensor@(sx, sy) beacon@(bx, by) = 
    let dsb  = manhattan sensor beacon 
        dy   = abs (sy - ly) 
        diff = dsb - dy
    in if diff < 0
         then Nothing 
         else Just (sx - diff, sx + diff)
       
parse :: String -> (Position, Position)
parse s = let ws = words s
              is = map (read . filter (\c -> c == '-' || isDigit c)) $ [ws !! 2, ws !! 3, ws !! 8, ws !! 9]
          in ((is !! 0, is !! 1), (is !! 2, is !! 3))

run :: String -> (String, String)
run s = 
    let pairs = map parse $ lines s 
        (sensors, beacons) = unzip pairs
        (ansy, maxy) = (2000000, 4000000)
        intervals = map fromJust $ filter isJust $ map (uncurry $ blocks ansy) pairs
        joined = join intervals 
        beaconsInside = count (\(bx, by) -> by == ansy && any (\(l, r) -> l <= bx && bx <= r) joined) (nub beacons)
        ans1 = sum (map intLength joined) - beaconsInside
        allJoined = map join $ map (\y -> map fromJust $ filter isJust $ map (uncurry $ blocks y) pairs) [0..maxy]
        match = head $ filter (\(y, xs) -> length xs > 1)$ zip [0..maxy] allJoined
        ans2 = (fst match) + (4000000 * (succ $ snd $ head $ sort $ snd match))
    in (show ans1, show ans2)