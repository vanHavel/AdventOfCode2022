module Days.Day1 where 

import Utils
import Data.Composition
import Data.Function
import Data.List

run :: String -> (String, String)
run s = 
    let grouped = filter (/= [""]) $ groupBy ((> 0) .: min `on` length) $ lines s 
        calories = map (sum . map read) $ grouped
    in both show (maximum calories, sum $ take 3 $ reverse . sort $ calories)