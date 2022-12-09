module Days.Day9 where 

import Control.Monad.State
import Data.List

import Grid
import Utils

data RopeState = RopeState {knots :: [Position]} deriving (Eq, Show)
initialState n = RopeState {knots=replicate n (0, 0)}

parseCommand :: String -> [Direction]
parseCommand s = let [a, b] = words s in replicate (read b) (read a)

moveRope :: Direction -> State RopeState Position 
moveRope dir = do 
    RopeState{knots=ks} <- get 
    let x:xs = ks
        nx = move x dir
        nknots = scanl1 (flip catchup) (nx:xs)
    put RopeState{knots=nknots}
    return $ last nknots

catchup :: Position -> Position -> Position 
catchup tp@(ty, tx) (hy, hx) | abs (ty - hy) <= 1 && abs (tx - hx) <= 1 = tp 
                             | otherwise = (ty + signum (hy - ty), tx + signum (hx - tx))

run :: String -> (String, String)
run s = let moves = concatMap parseCommand $ lines s 
        in both (show . length . nub . evalState (mapM moveRope moves) . initialState) (2, 10)