{-# LANGUAGE TupleSections #-}

module Days.Day8 where

import Data.Array
import Data.List

import Grid
import Utils

run :: String -> (String, String)
run s = let grid  = map (map $ read . singleton) $ lines s 
            n     = length grid
            agrid = listArray ((0, 0), (n-1, n-1)) $ concat grid
        in both show (length $ allVisible grid, maximum $ map (scenicScore agrid) [(y, x) | x <- [0..(n-1)], y <- [0..(n-1)]])

visibleLeft :: [Int] -> [Int]
visibleLeft xs = go (-1) $ zip [0..] xs
  where go _ [] = []
        go y ((i, x):xs) | y < x = i:(go x xs)
                         | True  = go y xs 

visibleRight :: [Int] -> [Int] 
visibleRight xs = let n = length xs 
                  in map (n-1-) $ visibleLeft $ reverse xs

allVisible :: [[Int]] -> [(Int, Int)]
allVisible grid = 
    let lefts  = concatMap (\(i, xs) -> map (i,) $ visibleLeft xs) $ zip [0..] grid
        rights = concatMap (\(i, xs) -> map (i,) $ visibleRight xs) $ zip [0..] grid 
        ups    = concatMap (\(i, xs) -> map (,i) $ visibleLeft xs) $ zip [0..] $ transpose grid 
        downs  = concatMap (\(i, xs) -> map (,i) $ visibleRight xs) $ zip [0..] $ transpose grid 
    in nub $ lefts ++ rights ++ ups ++ downs

scenicScore :: Array (Int, Int) Int -> (Int, Int) -> Int 
scenicScore grid pos = 
    let cur   = grid ! pos
        left  = seeFrom cur pos (0, -1)
        right = seeFrom cur pos (0, 1)
        up    = seeFrom cur pos (-1, 0)
        down  = seeFrom cur pos (1, 0)
    in left * right * up * down
        where seeFrom cur (py, px) move@(my, mx) = 
                let next = (py+my, px+mx) 
                in if inBounds (bounds grid) next 
                     then if (grid ! next) < cur 
                        then 1 + seeFrom cur next move
                        else 1
                     else 0 
                     
