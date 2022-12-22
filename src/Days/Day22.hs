module Days.Day22 where 

import Control.Monad.State
import Data.Array
import Data.Char

import Grid 
type Grid = Array Position Char

data Command = TurnLeft | TurnRight | Go Int deriving (Eq, Show)
type WalkState = (Position, Direction)

parseInstructions :: String -> [Command]
parseInstructions "" = []
parseInstructions ('L':xs) = TurnLeft:(parseInstructions xs)
parseInstructions ('R':xs) = TurnRight:(parseInstructions xs)
parseInstructions xs = (Go $ read $ takeWhile isDigit xs):(parseInstructions $ dropWhile isDigit xs)

parseGrid :: [String] -> Grid
parseGrid ls = 
    let my = length ls
        mx = maximum $ map length ls
    in listArray ((1, 1), (my, mx)) [c | l <- ls, c <- l ++ [' ' | _ <- [1..mx - length l]]]

initial :: Grid -> WalkState
initial grid = let (my, mx) = fst $ bounds grid in ((my, head [x | x <- [mx..], grid ! (my, x) == '.']), R)

step :: Bool -> Grid -> Command -> State WalkState ()
step cube grid c = do 
    cur@(pos, dir) <- get 
    case c of 
        TurnLeft -> put (pos, turnLeft dir)
        TurnRight -> put (pos, turnRight dir)
        Go i -> forM_ [1..i] $ \_ -> get >>= \cur -> modify' (advance cube grid cur)

advance :: Bool -> Grid -> WalkState -> WalkState -> WalkState
advance cube grid start (pos, dir) = 
    let (ny, nx) = move pos dir 
        ((miny, minx), (maxy, maxx)) = bounds grid
        cy = if ny < miny then maxy else if ny > maxy then miny else ny
        cx = if nx < minx then maxx else if nx > maxx then minx else nx 
    in case grid ! (cy, cx) of 
        ' ' -> if cube 
                then let (npos, ndir) = teleport ((cy, cx), dir) in case grid ! npos of
                    '.' -> (npos, ndir)
                    '#' -> start
                else advance cube grid start ((cy, cx), dir)
        '.' -> ((cy, cx), dir)
        '#' -> start

teleport :: WalkState -> WalkState 
teleport ((y, x), dir) | x >=  51 && x <= 100 && y == 201 && dir == U = ((x + 100, 1), R)
                       | y >= 151 && y <= 200 && x == 150 && dir == L = ((1, y - 100), D)
                       | x >=  51 && x <= 100 && y == 151 && dir == D = ((x + 100, 50), L)
                       | y >= 151 && y <= 200 && x == 51  && dir == R = ((150, y - 100), U)
                       | x >= 101 && x <= 150 && y == 201 && dir == U = ((200, 151 - x), U)
                       | x >=   1 && x <=  50 && y == 201 && dir == D = ((1, 151 - x), D)
                       | y >=   1 && y <=  50 && x ==   1 && dir == R = ((151 - y, 100), L)
                       | y >= 101 && y <= 150 && x == 101 && dir == R = ((151 - y, 150), L)
                       | x >= 101 && x <= 150 && y ==  51 && dir == D = ((x - 50, 100), L)
                       | y >=  51 && y <= 100 && x == 101 && dir == R = ((50, y + 50), U)
                       | y >=  51 && y <= 100 && x ==  50 && dir == L = ((101, y - 50), D)
                       | x >=   1 && x <=  50 && y == 100 && dir == U = ((x + 50, 51), R)
                       | y >= 101 && y <= 150 && x == 150 && dir == L = ((151 - y, 51), R)
                       | y >=   1 && y <=  50 && x ==  50 && dir == L = ((151 - y, 1), R)

faceScore :: Direction -> Int 
faceScore R = 0 
faceScore d = succ $ faceScore $ turnLeft d

run :: String -> (String, String)
run s = 
    let grid          = parseGrid $ init $ lines s 
        instructions  = parseInstructions $ last $ lines s 
        ((fy, fx), d) = execState (mapM_ (step False grid) instructions) (initial grid)
        ans1          = 1000 * fy + 4 * fx + faceScore d
        ((cy, cx), e) = execState (mapM_ (step True grid) instructions) (initial grid)
        ans2          = 1000 * cy + 4 * cx + faceScore e
    in (show ans1, show ans2)