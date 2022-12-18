module Grid3 where 

type Position = (Int, Int, Int)
data Direction = U | D | L | R | F | B deriving (Eq, Show, Read, Enum, Bounded)

allDirs :: [Direction]
allDirs = [minBound..maxBound]

move :: Position -> Direction -> Position 
move (x, y, z) d = case d of 
    U -> (x, y, z + 1)
    D -> (x, y, z - 1)
    L -> (x - 1, y, z)
    R -> (x + 1, y, z)
    F -> (x, y + 1, z)
    B -> (x, y - 1, z)

getX :: Position -> Int 
getX (x, _, _) = x

getY :: Position -> Int 
getY (_, y, _) = y

getZ :: Position -> Int 
getZ (_, _, z) = z