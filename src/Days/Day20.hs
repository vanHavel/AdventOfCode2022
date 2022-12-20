module Days.Day20 where 

type Zipper = ([(Int, Int)], [(Int, Int)])
toList :: Zipper -> [(Int, Int)]
toList (xs, ys) = reverse xs ++ ys
fromList :: [(Int, Int)] -> Zipper 
fromList xs = ([], xs)
len :: Zipper -> Int 
len (xs, ys) = length xs + length ys

mix :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
mix xs ys = toList $ foldl step ([], ys) xs

step :: Zipper -> (Int, Int) -> Zipper
step z i = let z' = find z i 
           in  move z' $ mod (snd i) (len z - 1)

find :: Zipper -> (Int, Int) -> Zipper
find (xs, []) i = find ([], reverse xs) i
find z@(xs, (y:ys)) i = if i == y then z else find ((y:xs), ys) i

move :: Zipper -> Int -> Zipper
move z 0 = z 
move (xs, ys) i | i > 0 = case ys of 
    [y] -> move ([], y:(reverse xs)) i
    (y:y':ys) -> move (y':xs, y:ys) (i-1)
                | i < 0 = case xs of 
    [] -> move (reverse $ tail ys, [head ys]) i
    (x:xs) -> move (xs, (head ys):x:(tail ys)) (i+1)

advance :: Zipper -> Int -> Zipper 
advance (xs, []) i = advance ([], reverse xs) i 
advance z 0 = z 
advance (xs, y:ys) i = advance (y:xs, ys) (i-1)

run :: String -> (String, String)
run s = 
    let ints  = zip [1..] $ map read $ lines s 
        [zz]  = filter ((== 0) . snd) ints
        mixed = mix ints ints
        zzip  = find (fromList mixed) zz
        ans1  = sum $ map (\i -> snd $ head $ snd $ advance zzip (1000 * i)) [1..3]
        decs  = [(j, i * 811589153) | (j, i) <- ints]
        mix2d = iterate (mix decs) decs !! 10
        zzip2 = find (fromList mix2d) zz
        ans2  = sum $ map (\i -> snd $ head $ snd $ advance zzip2 (1000 * i)) [1..3]
    in (show ans1, show ans2)