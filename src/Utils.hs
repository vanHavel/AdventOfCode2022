module Utils where

import Data.Bifunctor
import Data.Composition
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