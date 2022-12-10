module Days.Day10 where

import Data.Composition
import Data.List
import Data.List.Split

import Utils

data Command = AddX Int | NoOp deriving (Eq, Show)

parse :: String -> Command 
parse s = let (opcode, rest) = splitAt 4 s 
          in case opcode of 
            "addx" -> AddX (read rest)
            "noop" -> NoOp

execute :: Int -> Command -> [Int]
execute i (AddX j) = [i, i + j]
execute i NoOp     = [i]

pixel :: Bool -> Char 
pixel True  = '#'
pixel False = '.'

run :: String -> (String, String)
run s = let commands = map parse $ lines s 
            states   = concat $ scanl (execute . last) [1] commands 
            relevant = every 40 $ drop 19 states 
            ans1     = sum $ zipWith (*) relevant [20, 60..220]
            ls       = take 6 $ chunksOf 40 states 
            rows     = map (map pixel . zipWith ((<= 1) . abs .: subtract) [0..]) ls 
        in (show ans1, concat $ intersperse "\n" rows)

