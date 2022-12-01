module Days.Day1 where 

import Data.Composition
import Data.Function
import Data.List

run :: String -> (String, String)
run s = 
    let grouped = filter (/= [""]) $ groupBy ((> 0) .: min `on` length) $ lines s 
        calories = map (sum . map read) $ grouped
    in (show $ maximum calories, show $ sum $ take 3 $ reverse . sort $ calories)