{-# LANGUAGE TupleSections #-}
module Days.Day14 where 

import Data.Array
import Data.Containers.ListUtils
import Data.List.Split 
import Debug.Trace

import Grid
import Utils 

type Grid = Array Position Char

parse :: String -> [Position]
parse s = 
    let points = map ((\[a, b] -> both read (a, b)) . splitOn ",") $ filter (/= "->") $ words s
        pairs  = (zip points $ tail points)
    in nubOrd $ concatMap path pairs 
        where path ((x1, y1), (x2, y2)) | x1 == x2 = map (x1,) [(min y1 y2)..(max y1 y2)]
                                        | y1 == y2 = map (,y1) [(min x1 x2)..(max x1 x2)]

dropSand :: Grid -> Position -> Maybe Position 
dropSand grid (x, y) = 
    if not $ inBounds (bounds grid) (x, y + 1)
        then Nothing
    else if grid ! (x, y + 1) == '.' 
        then dropSand grid (x, y + 1)
    else if grid ! (x - 1, y + 1) == '.'
        then dropSand grid (x - 1, y + 1)
    else if grid ! (x + 1, y + 1) == '.'
        then dropSand grid (x + 1, y + 1)
    else 
        Just (x, y)
    

simulate :: Grid -> Int 
simulate grid = case dropSand grid (500, 0) of 
                    Nothing       -> 0 
                    Just (500, 0) -> 1
                    Just pos      -> 1 + simulate (grid // [(pos, 'o')])

run :: String -> (String, String)
run s = 
    let rocks  = concatMap parse $ lines s 
        bounds@((minx, miny), (maxx, maxy)) = (
            ((minimum $ map fst rocks) - 500, 0), 
            ((maximum $ map fst rocks) + 500, (maximum $ map snd rocks) + 2)
            )
        grid   = listArray bounds ['.' | i <- [minx..maxx], j <- [miny..maxy]] // [(p, '#') | p <- rocks]
        grid2  = grid // [((x, maxy), '#') | x <- [minx..maxx]]
    in (show $ simulate grid, show $ simulate grid2)