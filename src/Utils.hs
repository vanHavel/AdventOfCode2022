module Utils where

import Data.Bifunctor
import Data.Composition
import Data.Function
import Data.List.Split

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

twice :: (a -> a) -> (a -> a)
twice f = f . f

count :: (a -> Bool) -> [a] -> Int 
count = length .: filter

splittingParse :: String -> (String -> a) -> String -> (a, a)
splittingParse splitter partParser s = 
    let [left, right] = splitOn splitter s 
    in both partParser (left, right)

every :: Int -> [a] -> [a]
every k = every' k 1
  where every' _ _ [] = []
        every' k 1 (x:xs) = x:(every' k k xs)
        every' k i (x:xs) = every' k (i-1) xs

self :: (a -> a -> a) -> a -> a
self f x = f x x

mean :: [Int] -> Int 
mean xs = div (sum xs) (length xs)