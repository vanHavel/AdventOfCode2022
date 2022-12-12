module Days.Day12 where 

import AStar
import Grid
import Utils 

import Data.Array
import Data.Char
import qualified Data.Set as Set

run :: String -> (String, String)
run s = 
    let grid      = lines s 
        (n, m)    = (length grid, length $ grid !! 0)
        agrid     = listArray ((0, 0), (n-1, m-1)) $ concat grid
        [init]   = posWhere (== 'S') agrid
        [final]  = posWhere (== 'E') agrid
        agrid'    = agrid // [(init, 'a'), (final, 'z')]
        h1        = \pos -> (ord 'z') - (ord (agrid' ! pos))
        succFun1  = \pos -> Set.fromList [(npos, 1) | dir <- allDirs, let npos = move pos dir, inBounds (bounds agrid') npos,
                                                      (ord $ agrid' ! npos) - (ord $ agrid' ! pos) <= 1]                                          
        h2        = \pos -> (ord (agrid' ! pos)) - (ord 'a')
        succFun2  = \pos -> Set.fromList [(npos, 1) | dir <- allDirs, let npos = move pos dir, inBounds (bounds agrid') npos,
                                                      (ord $ agrid' ! npos) - (ord $ agrid' ! pos) >= (-1)]       
        goalTest  = \pos -> agrid' ! pos == 'a'                                   
        cost1     = aStarSearch init final h1 succFun1
        cost2     = fst $ genAStarSearch final h2 succFun2 goalTest

    in both show (cost1, cost2)
