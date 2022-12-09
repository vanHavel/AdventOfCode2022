module Grid where 

type Position = (Int, Int)
data Direction = R | U | L | D deriving (Eq, Show, Read)

move :: Position -> Direction -> Position 
move (py, px) U = (py - 1, px)
move (py, px) D = (py + 1, px)
move (py, px) L = (py, px - 1)
move (py, px) R = (py, px + 1)