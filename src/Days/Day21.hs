module Days.Day21 where 

import Data.Char
import Data.List.Split
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Debug.Trace

data Tree = Leaf Int | Human Int | Add Tree Tree | Sub Tree Tree | Mul Tree Tree | Div Tree Tree | Root Tree Tree deriving (Eq, Ord, Show)

build :: String -> Map String String -> Tree
build "humn" m = Human $ read $ m ! "humn"
build root m = let line = m ! root in 
    if all isDigit line then Leaf (read line)
    else let [left, op, right] = words line 
             (recl, recr)     = (build left m, build right m)
         in if root == "root" then Root recl recr else 
            case op of "+" -> Add recl recr 
                       "-" -> Sub recl recr 
                       "*" -> Mul recl recr 
                       "/" -> Div recl recr

hasHuman :: Tree -> Bool 
hasHuman (Leaf _) = False; hasHuman (Human _) = True 
hasHuman (Add l r) = hasHuman l || hasHuman r; hasHuman (Sub l r) = hasHuman l || hasHuman r
hasHuman (Mul l r) = hasHuman l || hasHuman r; hasHuman (Div l r) = hasHuman l || hasHuman r
hasHuman (Root l r) = hasHuman l || hasHuman r

calcHuman :: Tree -> Int -> Int 
calcHuman (Human _) i = i
calcHuman (Root l r) i = if hasHuman l then calcHuman l (eval r) else calcHuman r (eval l)
calcHuman (Add l r) i = if hasHuman l then calcHuman l (i - eval r) else calcHuman r (i - eval l)
calcHuman (Sub l r) i = if hasHuman l then calcHuman l (i + eval r) else calcHuman r (eval l - i)
calcHuman (Mul l r) i = if hasHuman l then calcHuman l (i `div` eval r) else calcHuman r (i `div` eval l)
calcHuman (Div l r) i = if hasHuman l then calcHuman l (i * eval r) else calcHuman r (eval l `div` i)

eval :: Tree -> Int 
eval (Human i) = i; eval (Leaf i) = i 
eval (Add l r) = eval l + eval r; eval (Sub l r) = eval l - eval r
eval (Mul l r) = eval l * eval r; eval (Div l r) = eval l `div` eval r
eval (Root l r) = eval (Add l r)

run :: String -> (String, String)
run s = 
    let input = Map.fromList [(name, op) | line <- lines s, let [name, op] = splitOn ": " line]
        tree  = build "root" input 
        ans1  = eval tree
        ans2  = calcHuman tree 0
    in (show ans1, show ans2)