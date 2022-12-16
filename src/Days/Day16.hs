module Days.Day16 where 

import Data.Char
import qualified Data.Map as Map 
import Data.Map (Map, (!))
import Data.List
import qualified Data.Set as Set 
import Data.Set (Set)
import Debug.Trace

data ValveState = ValveState {
    pos :: String,
    time :: Int,
    open :: Set String
    } deriving (Eq, Ord, Show)

parseAdj :: String -> Map String [(Int, String)]
parseAdj s = 
    let edges = [(l, r) | line <- lines s, let l = (words line) !! 1, r <- drop 9 $ words $ filter (/= ',') line]
    in Map.fromList [(r, [(1, l) | (l, rr) <- edges, rr == r]) | r <- nub $ map snd edges] 

parseValves :: String -> Map String Int 
parseValves s = Map.fromList [(pos, val) | line <- lines s, let pos = (words line) !! 1, let val = read $ filter isDigit line, val > 0]

reduce :: Map String [(Int, String)] -> [String] -> Map String [(Int, String)] 
reduce adj rel = Map.fromList [(l, [(d, r) | (d, r) <- distances adj l, r /= l, r `elem` rel]) | l <- rel]

distances :: Map String [(Int, String)] -> String -> [(Int, String)] 
distances adj start = bfs [(0, start)] [(0, start)]
  where bfs ::[(Int, String)] -> [(Int, String)]  -> [(Int, String)] 
        bfs [] ys = ys 
        bfs ((dist, name):xs) ys = 
            let succs = map snd $ adj ! name
                ok    = filter (\s -> not $ s `elem` (map snd (ys))) succs
                done  = [(dist + 1, n) | n <- ok]
            in bfs (xs ++ done) (ys ++ done)

solve :: Map String [(Int, String)] -> Map String Int -> Int -> Map ValveState Int 
solve adj valves maxT = dp where 
    dp = Map.fromList [(vs, go vs) | p <- (Map.keys valves) ++ ["AA"], t <- [1..maxT], 
                                     o <- Set.toList $ Set.powerSet $ Set.fromList $ Map.keys valves,
                                     let vs = ValveState {pos=p, time=t, open=o}
                          ]
    go ValveState {pos=p, time=t, open=o} | t == 1 && p == "AA" && o == Set.empty = 0 
                                          | t == 1 = -1000000
                                          | otherwise =
        let moves = [(d, ValveState{pos=np, time=t - d, open=o}) | (d, np) <- adj ! p, t - d > 0]
            act   = if Map.member p valves && Set.member p o then [(1, ValveState {pos=p, time=pred t, open=Set.delete p o})] else []
            wait  = if t == maxT then [(d, ValveState{pos=p, time=t - d, open=o}) | d <- [1..(maxT - 1)]] else []
            flow  = sum $ Set.map (valves !) o
            recs  = [flow * d + dp ! prev | (d, prev) <- moves ++ act ++ wait]
        in if recs == [] then -1000000 else maximum recs

run :: String -> (String, String)
run s = 
    let adj    = parseAdj s 
        valves = parseValves s 
        radj   = reduce adj $ ["AA"] ++ Map.keys valves
        dp     = solve radj valves 30
        ans1   = maximum $ Map.elems $ Map.filterWithKey (\vs _ -> time vs == 30) dp
        dp2    = solve radj valves 26
        ends   = Map.filterWithKey (\vs i -> time vs == 26 && i > 500 && length (open vs) > 2) dp2
        ans2   = maximum [i1 + i2 | (vs1, i1) <- Map.assocs ends, (vs2, i2) <- Map.assocs ends, Set.intersection (open vs1) (open vs2) == Set.empty]
    in (show ans1, show ans2)