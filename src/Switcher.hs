module Switcher(runDay) where

import Days.Day1 (run)
import Days.Day2 (run)
import Days.Day3 (run)
import Days.Day4 (run)
import Days.Day5 (run)
import Days.Day6 (run)
import Days.Day7 (run)
import Days.Day8 (run)
import Days.Day9 (run)
import Days.Day10 (run)
import Days.Day11 (run)
import Days.Day12 (run)
import Days.Day13 (run)
import Days.Day14 (run)
import Days.Day15 (run)
import Days.Day16 (run)

runDay :: Int -> String -> (String, String)
runDay 1 = Days.Day1.run
runDay 2 = Days.Day2.run
runDay 3 = Days.Day3.run
runDay 4 = Days.Day4.run
runDay 5 = Days.Day5.run
runDay 6 = Days.Day6.run
runDay 7 = Days.Day7.run
runDay 8 = Days.Day8.run
runDay 9 = Days.Day9.run
runDay 10 = Days.Day10.run
runDay 11 = Days.Day11.run
runDay 12 = Days.Day12.run
runDay 13 = Days.Day13.run
runDay 14 = Days.Day14.run
runDay 15 = Days.Day15.run
runDay 16 = Days.Day16.run