module Utils where

import Data.Bifunctor
import Data.Composition

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

twice :: (a -> a) -> (a -> a)
twice f = f . f

count :: (a -> Bool) -> [a] -> Int 
count = length .: filter