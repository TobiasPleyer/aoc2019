module Aoc.Day1
    ( solution
    ) where


import Aoc.Common (DailyChallenge(..))


getInput
  :: IO [Int]
getInput = (map read . lines) <$> readFile "inputs/day1.txt"


calculateFuel mass = (mass `div` 3) - 2


calculateTotalFuel = sum . takeWhile (>0) . drop 1 . iterate calculateFuel


solution_p1 input = return $ show $ sum (map calculateFuel input)


solution_p2 input = return $ show $ sum (map calculateTotalFuel input)


solution :: IO DailyChallenge
solution = do
  input <- getInput
  return $ DailyChallenge (solution_p1 input) (Just (solution_p2 input))
