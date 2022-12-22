module Grid where 

import Data.Array
import Data.Function

type Position = (Int, Int)
data Direction = R | U | L | D deriving (Eq, Show, Read, Enum, Bounded)

allDirs :: [Direction]
allDirs = [minBound..maxBound]

move :: Position -> Direction -> Position 
move (py, px) U = (py - 1, px)
move (py, px) D = (py + 1, px)
move (py, px) L = (py, px - 1)
move (py, px) R = (py, px + 1)

turnLeft :: Direction -> Direction 
turnLeft U = L; turnLeft L = D; turnLeft D = R; turnLeft R = U 
turnRight :: Direction -> Direction 
turnRight = turnLeft . turnLeft . turnLeft

inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool 
inBounds ((ly, lx), (uy, ux)) (y, x) = lx <= x && x <= ux && ly <= y && y <= uy

posWhere :: (Ix i) => (e -> Bool) -> Array i e -> [i]
posWhere f = map fst . filter (f . snd) . assocs

manhattan :: Position -> Position -> Int 
manhattan (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2)

