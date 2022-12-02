module Utils where

import Data.Bifunctor

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

twice :: (a -> a) -> (a -> a)
twice f = f . f