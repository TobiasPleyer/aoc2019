module Aoc.Common where


data DailyChallenge =
  DailyChallenge
  { part1 :: IO String
  , part2 :: Maybe (IO String)
  }


isDebug = False
