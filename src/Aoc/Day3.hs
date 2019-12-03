module Aoc.Day3
    ( solution
    ) where


import Aoc.Common (DailyChallenge(..), isDebug)


getInput
  :: IO [Int]
getInput = (read . makeList) <$> readFile "inputs/day2.txt"
  where makeList s = "[" ++ s ++ "]"


--solution_p1 :: [Int] -> IO String
solution_p1 input = return "N/A"


--solution_p2 :: [Int] -> IO String
solution_p2 input = return "N/A"


solution :: IO DailyChallenge
solution = do
  input <- getInput
  return $ DailyChallenge (solution_p1 input) Nothing
