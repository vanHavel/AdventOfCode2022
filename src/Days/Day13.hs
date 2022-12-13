module Days.Day13 where
    
import Data.Char
import Data.List
import Data.List.Split

import Utils

data Packet = Number Int | List [Packet]
instance Show Packet where 
    show (Number i) = show i 
    show (List ps)  = show ps
instance Eq Packet where 
    (Number i) == (Number j) = i == j 
    (List xs)  == (List ys)  = xs == ys
    (Number i) == (List [j]) = (Number i) == j
    (Number i) == (List _)   = False
    x == y = y == x
instance Ord Packet where
    (Number i)    <= (Number j)    = i <= j
    (List [])     <= (List _)      = True
    (List (x:xs)) <= (List [])     = False 
    (List (x:xs)) <= (List (y:ys)) = x < y || (x == y && (List xs) <= (List ys))
    (Number i)    <= (List xs)     = (List [Number i]) <= (List xs)
    (List xs)     <= (Number i)    = (List xs) <= (List [Number i])

parse :: String -> Packet 
parse = fst . parsePacket 
    where parseElements (']':xs) = ([], xs) 
          parseElements (',':xs) = parseElements xs
          parseElements ('[':xs) = let (p, ys)  = parsePacket ('[':xs) 
                                       (es, zs) = parseElements ys
                                   in  ((p:es), zs)
          parseElements xs       = let (n, ys)  = parseNumber xs 
                                       (es, zs) = parseElements ys
                                   in  ((n:es), zs)
          parsePacket ('[':xs)   = let (pk, ys) = parseElements xs 
                                   in  (List pk, ys)
          parseNumber xs         = let (ns, ys) = span isDigit xs in (Number $ read ns, ys)

run :: String -> (String, String)
run s = 
    let parsed = map parse $ filter (/= "") $ lines s
        pairs  = chunksOf 2 parsed
        zipped = zip [1..] pairs
        good   = filter (\(i, [a, b]) -> a < b) zipped
        ans1   = sum $ map fst good 
        added  = [List [List [Number 2]], List [List [Number 6]]]
        sorted = sort (parsed ++ added)
        ans2   = product $ map fst $ filter (\x -> snd x `elem` added) $ zip [1..] sorted
    in (show ans1, show ans2)
        