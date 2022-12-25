module Days.Day25 where 

convertC :: Char -> Int 
convertC '-' = -1; convertC '=' = -2; convertC c = read [c]

convertI :: Int -> Char 
convertI 3 = '='; convertI 4 = '-'; convertI i = head $ show i

convert :: String -> Int 
convert [] = 0 
convert (c:cs) = (convertC c) * (5 ^ (length cs)) + convert cs

back :: Int -> String 
back i | i <= 2 = show i
       | otherwise = let rem = i `mod` 5 
                         c = convertI rem 
                     in if rem > 2 then c:(back $ succ $ div i 5) else c:(back $ div i 5)

run :: String -> (String, String)
run s = 
    let ansN = sum $ map convert $ lines s 
    in (show $ ansN, reverse $ back ansN)