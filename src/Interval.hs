module Interval where 

type Interval = (Int, Int)

intLength :: Interval -> Int
intLength (mi, ma) = ma - mi + 1

intersect :: Interval -> Interval -> Maybe Interval
(l1, r1) `intersect` (l2, r2) | l > r     = Nothing 
                              | otherwise = Just (l, r)
    where (l, r) = (max l1 l2, min r1 r2)

union :: Interval -> Interval -> Interval 
(l1, r1) `union` (l2, r2) = (min l1 l2, max r1 r2)