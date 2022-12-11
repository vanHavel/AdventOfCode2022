module Days.Day11 where 

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map 
import Data.Map.Strict (Map, (!))
import Data.List

import Utils

ops :: [Int -> Int]
ops = [(* 11), (+ 8), (+ 1), (* 7), (+ 4), (+ 7), (self (*)), (+ 6)]
n = length ops

mods :: [Int] 
mods = [19, 2, 3, 17, 13, 7, 5, 11]
modulus = product mods

tests :: [Int -> Int]
tests = [modTest 19 6 7, modTest 2 6 0, modTest 3 5 3, modTest 17 5 4, modTest 13 0 1, modTest 7 1 4, modTest 5 7 2, modTest 11 2 3]
    where modTest m t f x = if mod x m == 0 then t else f

data MonkeyState = MonkeyState {items :: Map Int [Int], inspections :: Map Int Int}
initialState = MonkeyState {
    items=Map.fromList $ zip [0..] [[74, 73, 57, 77, 74], [99, 77, 79], [64, 67, 50, 96, 89, 82, 82], [88], [80, 66, 98, 83, 70, 63, 57, 66], [81, 93, 90, 61, 62, 64], [69, 97, 88, 93], [59, 80]], 
    inspections=Map.fromList $ zip [0..(n-1)] (repeat 0)
    }

runTurn :: Bool -> Int -> State MonkeyState ()
runTurn divide monkey = do 
    is <- (! monkey) <$> items <$> get  
    modify' (\s -> s{
        items=Map.insert monkey [] (items s), 
        inspections=Map.alter (fmap (+ (length is))) monkey (inspections s)
        })
    forM_ is $ \item -> do 
        let applied = ((ops !! monkey) item) 
            reduced = if divide then applied `div` 3 else applied `mod` modulus
            target  = (tests !! monkey) reduced 
        modify' (\s -> s{items=Map.alter (fmap $ (++ [reduced])) target (items s)})

runRound :: Bool -> State MonkeyState () 
runRound divide = forM_ [0..(n-1)] (\monkey -> runTurn divide monkey)

run :: String -> (String, String)
run _ = let finalState1  = execState (sequence_ $ replicate 20 $ runRound True) initialState
            [m1, m2]     = take 2 $ reverse . sort $ Map.elems $ inspections finalState1
            finalState2  = execState (sequence_ $ replicate 10000 $ runRound False) initialState
            [m3, m4]     = take 2 $ reverse . sort $ Map.elems $ inspections finalState2
        in (show $ m1 * m2, show $ m3 * m4)