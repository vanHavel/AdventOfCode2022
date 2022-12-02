module Switcher(runDay) where

import Days.Day1 (run)
import Days.Day2 (run)

runDay :: Int -> String -> (String, String)
runDay 1 = Days.Day1.run
runDay 2 = Days.Day2.run