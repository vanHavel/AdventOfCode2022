module Main where

import System.Environment ( getArgs )

import Switcher ( runDay )

main :: IO ()
main = do
  day <- head <$> getArgs
  input <- readFile $ "data/day" ++ day ++ ".txt"
  let (s1, s2) = Switcher.runDay (read day) input
  putStrLn s1 
  putStrLn s2
