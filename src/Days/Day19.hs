{-# LANGUAGE BangPatterns #-}
module Days.Day19 where 

import Data.Char
import qualified Data.Set as Set 
import Data.Set (Set)
import Debug.Trace

import Utils

data BluePrint = BluePrint {
    oreCost  :: Int,
    clayCost :: Int,
    obsCost  :: (Int, Int),
    geoCost  :: (Int, Int)
} deriving (Eq, Ord, Show)

data Node = Node {
    oreRobots  :: !Int, 
    clayRobots :: !Int, 
    obsRobots  :: !Int,
    geoRobots  :: !Int,
    ore        :: !Int,
    clay       :: !Int,
    obs        :: !Int,
    geo        :: !Int,
    time       :: !Int
    } deriving (Eq, Ord, Show)

initial :: Node
initial = Node{oreRobots=1, clayRobots=0, obsRobots=0, geoRobots=0, ore=0, clay=0, obs=0, geo=0, time=1}

succFun :: BluePrint -> Node -> [Node]
succFun print node = succs (wait node)
    where succs waited = [waited]
            ++ (if ore node >= oreCost print && oreRobots node <= maximum [oreCost print, clayCost print, fst $ obsCost print, fst $ geoCost print]
                then [waited{ore=ore waited - oreCost print, oreRobots=oreRobots waited + 1}] 
                else [])
            ++ (if ore node >= clayCost print && clayRobots node <= snd (obsCost print)
                then [waited{ore=ore waited - clayCost print, clayRobots=clayRobots waited + 1}] 
                else [])
            ++ (if ore node >= fst (obsCost print) && clay node >= snd (obsCost print) && obsRobots node <= snd (geoCost print)
                then [waited{ore=ore waited - fst (obsCost print), clay=clay waited - snd (obsCost print), obsRobots=obsRobots waited + 1}]
                else [])
            ++ (if ore node >= fst (geoCost print) && obs node >= snd (geoCost print)
                then [waited{ore=ore waited - fst (geoCost print), obs=obs waited - snd (geoCost print), geoRobots=geoRobots waited + 1}]
                else [])

wait :: Node -> Node 
wait node@Node{oreRobots=or, clayRobots=cr, obsRobots=lr, geoRobots=gr, ore=o, clay=c, obs=l, geo=g, time=t} = 
    node{time=succ t, ore=min 10 $ o+or, clay=min 40 $ c+cr, obs=min 20 $ l+lr, geo=g+gr}

dfs :: Int -> BluePrint -> Set Node -> [Node] -> Set Node 
dfs _ _ seen [] = seen
dfs maxTime print seen (x:xs) = 
    let succs = filter (\s -> time s <= maxTime && not (Set.member s seen)) $ succFun print x 
    in dfs maxTime print (Set.union seen $ Set.fromList (x:succs)) (succs ++ xs)

parse :: String -> BluePrint
parse s = let is = map read $ filter (all isDigit) $ words s 
          in  BluePrint{oreCost=is !! 0, clayCost= is!! 1, obsCost=(is !! 2, is !! 3), geoCost=(is !! 4, is !! 5)}

run :: String -> (String, String)
run s = let blueprints = map parse $ lines s
            goals1     = [dfs 25 print Set.empty [initial] | print <- blueprints]
            scores1    = map (maximum . Set.map geo) goals1
            ans1       = sum $ zipWith (*) [1..] scores1
            goals2     = [dfs 33 print Set.empty [initial] | print <- take 3 blueprints]
            scores2    = map (maximum . Set.map geo) goals2
            ans2       = product scores2
        in both show (ans1, ans2)