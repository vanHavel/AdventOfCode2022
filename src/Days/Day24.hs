module Days.Day24 where 

import qualified Data.Map.Strict as Map 
import Data.Map.Strict (Map)
import qualified Data.Set as Set 
import Data.Set (Set)

import AStar
import Grid

type Blizzard = (Position, Direction)
type Bounds = ((Int, Int), (Int, Int))
type NodeState = (Position, Int)

parseDir :: Char -> Direction
parseDir '>' = R; parseDir '<' = L; parseDir '^' = U; parseDir 'v' = D

step :: Bounds -> Blizzard -> Blizzard 
step ((miny, minx), (maxy, maxx)) (pos, dir) = 
    let (cy, cx) = move pos dir 
    in  ((
            if cy < miny then maxy else if cy > maxy then miny else cy, 
            if cx < minx then maxx else if cx > maxx then minx else cx
    ), dir)

blizzards :: Bounds -> Set Blizzard -> Map Int (Set Blizzard)
blizzards bounds = Map.fromList . take 1000 . zip [0..] . iterate (Set.map (step bounds))

succs :: Bounds -> Position -> Map Int (Set Blizzard) -> NodeState -> Set (NodeState, Int)
succs bounds fpos bs (pos, turn) = 
    let cur    = Set.map fst $ bs Map.! (turn + 1)
        nposs  = pos:[npos | dir <- allDirs, let npos = move pos dir, inBounds bounds npos || npos == fpos]
        valids = filter (\p -> not $ Set.member p cur) nposs
    in Set.fromList [((valid, succ turn), 1) | valid <- valids]

h :: Position -> NodeState -> Int 
h fpos (pos, _) = manhattan pos fpos

run :: String -> (String, String)
run s = 
    let ls      = lines s
        initpos = (0, head $ map fst $ filter ((== '.') . snd) $ zip [0..] $ head ls)
        fpos    = (subtract 1 $ length ls, head $ map fst $ filter ((== '.') . snd) $ zip [0..] $ last ls)
        bounds  = ((1, 1), (subtract 2 $ length ls, subtract 2 $ length $ head ls))
        bset    = Set.fromList [((y, x), parseDir c) | (y, line) <- zip [0..] ls, (x, c) <- zip [0..] line, not $ c `elem` ".#"]
        blizz   = blizzards bounds bset
        cost1   = fst $ genAStarSearch (initpos, 0) (h fpos) (succs bounds fpos blizz) ((== fpos) . fst)
        cost2   = fst $ genAStarSearch (fpos, cost1) (h initpos) (succs bounds initpos blizz) ((== initpos) . fst)
        cost3   = fst $ genAStarSearch (initpos, cost1 + cost2) (h fpos) (succs bounds fpos blizz) ((== fpos) . fst)
    in (show cost1, show $ cost1 + cost2 + cost3)