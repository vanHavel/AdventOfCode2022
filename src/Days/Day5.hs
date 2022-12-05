module Days.Day5 where

import Utils

data Instruction = Instruction {quantity :: Int, source :: Int, target :: Int}
type State = [[Char]]

runStep :: Bool -> Instruction -> State -> State 
runStep doReverse instruction state = 
    let toMove = take (quantity instruction) (state!!(source instruction))
    in runStep' (if doReverse then reverse else id) instruction toMove state 
        where runStep' _ _ _ [] = []
              runStep' moveFunc instruction toMove (s:ss) =
                let rec = runStep' moveFunc (dec instruction) toMove ss
                in  if source instruction == 0 
                      then (drop (quantity instruction) s):rec 
                    else if target instruction == 0
                      then ((moveFunc toMove) ++ s):rec
                    else 
                      s:rec
              dec instruction = instruction{source = pred $ source instruction, target = pred $ target instruction }

runSteps :: Bool -> State -> [Instruction] -> State 
runSteps doReverse = foldl (flip (runStep doReverse))

parseInstruction :: String -> Instruction 
parseInstruction s = Instruction{quantity = read (w!!1), source = pred $ read (w!!3), target = pred $ read (w!!5)}
    where w = words s

initialState :: State 
initialState = [
        "LCGMQ", "GHFTCLDR", "RWTMNFJV", "PQVDFJ", "TBLSMFN", "PDCHVNR", "TCH", "PHNZVJSG", "GHFZ"
    ]

run :: String -> (String, String)
run s = 
    let instructions = map parseInstruction $ lines s 
        finalState1  = runSteps True initialState instructions
        finalState2  = runSteps False initialState instructions
    in both (map head) (finalState1, finalState2)