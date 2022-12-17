module Days.Day17 where 

import Control.Monad
import Control.Monad.State 
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.List.Split
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Debug.Trace
import Safe.Foldable

import Grid 
import Utils

data GameState = GameState {cur :: Set Position, fixed :: Set Position, commands :: Array Int Direction, pointer :: Int} deriving (Eq, Show)

gridBounds :: ((Int, Int), (Int, Int))
gridBounds = ((0, 0), (1000000, 6))

rocks :: [[Position]]
rocks = [
    [(0, 0), (0, 1), (0, 2), (0, 3)],
    [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
    [(0, 0), (0, 1), (0, 2), (1, 2), (2, 2)],
    [(0, 0), (1, 0), (2, 0), (3, 0)],
    [(0, 0), (1, 0), (0, 1), (1, 1)]
    ]

dropBlock :: Int -> State GameState Int
dropBlock typ = do 
    fixedFields <- fixed <$> get
    let spawnY = (+4) $ fromMaybe 0 $ maximumMay $ Set.map fst fixedFields
        spawnX = 2
        spawn  = Set.fromList [(spawnY + by, spawnX + bx) | (by, bx) <- rocks !! typ]
    modify' (\s -> s{cur=spawn})
    steps
    s <- get 
    modify' (\s -> s{fixed=Set.union (cur s) (fixed s), cur=Set.empty})
    maximum <$> Set.map fst <$> fixed <$> get

steps :: State GameState () 
steps = do 
    shift
    s <- get 
    if all (\pos -> inBounds gridBounds (move pos U) && not (Set.member (move pos U) (fixed s))) (cur s)
        then (fall >> steps)
        else return ()

shift :: State GameState ()
shift = do 
    GameState{commands=cmds, pointer=p, cur=c, fixed=f} <- get 
    let cmd = cmds ! p
        np  = if p == snd (Array.bounds cmds) then fst (Array.bounds $ cmds) else succ p
    modify' (\s -> s{pointer=np})
    when (all (\pos -> inBounds gridBounds (move pos cmd) && not (Set.member (move pos cmd) f)) c) $ 
        modify' (\s -> s{cur=Set.map (\pos -> move pos cmd) c})

fall :: State GameState () 
fall = modify' (\s -> s{cur=Set.map (\pos -> move pos U) (cur s)})

parse :: Char -> Direction 
parse '<' = L; parse '>' = R;

run :: String -> (String, String)
run s = 
    let cmds    = Array.listArray (1, length s) (map parse s)
        initial = GameState{cur=Set.empty, fixed=Set.fromList [(0, x) | x <- [0..6]], commands=cmds, pointer=1}
        heights = evalState (mapM dropBlock (concat $ repeat [0..4])) initial
        ans1    = heights !! 2021
        diffs   = zipWith subtract (0:heights) heights
        reduced = map sum $ chunksOf 5 diffs
        (mu, p) = head $ [(mu, p) | mu <- [1..500], p <- [10..500], let rest = drop mu reduced, take p rest == take p (drop p rest)]
        pInc    = sum $ take p $ drop mu reduced
        (t, r) = divMod (200000000000 - mu) p
        ans2    = t * pInc + sum (take mu reduced) + sum (take r $ drop mu reduced)
    in (show ans1, show ans2)