module Aoc.Day1
    ( solution
    , calculateFuel
    ) where


calculateFuel mass = (mass `div` 3) - 2

calculateTotalFuel = sum . takeWhile (>0) . drop 1 . iterate calculateFuel

getInput
  :: IO [Int]
getInput = (map read . lines) <$> readFile "inputs/day1.txt"


solution :: IO ()
solution = do
  input <- getInput
  let answer1 = sum (map calculateFuel input)
  putStrLn $ "  Part 1: " ++ show answer1
  let answer2 = sum (map calculateTotalFuel input)
  putStrLn $ "  Part 2: " ++ show answer2
