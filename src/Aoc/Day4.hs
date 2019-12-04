module Aoc.Day4 where
--    ( solution
--    ) where


import Control.Arrow ((***))
import Data.Digits (digits)
import Data.List (break)

import Aoc.Common (DailyChallenge(..), isDebug)


getInput
  :: IO (Int,Int)
getInput = ((read *** (read . tail)) . break (=='-')) <$> readFile "inputs/day4.txt"


getDigits = digits 10


checkCriteria :: [(Int -> Bool)] -> Int -> Bool
checkCriteria criteria number = and $ map ($ number) criteria


isSixDigit :: Int -> Bool
isSixDigit n = (n>=100000) && (n <= 999999)


withinRange :: Int -> Int -> Int -> Bool
withinRange lower upper n = (lower <= n) && (n <= upper)


hasAdjacentDigits :: Int -> Bool
hasAdjacentDigits n
  | n < 10 = False
  | otherwise = any (uncurry (==)) (zip (init ds) (tail ds))
  where ds = getDigits n


digitsDontDecrease :: Int -> Bool
digitsDontDecrease n = all (\(a,b) -> a <= b) (zip (init ds) (tail ds))
  where ds = getDigits n


--solution_p1 :: ([Direction Int],[Direction Int]) -> IO String
solution_p1 (start,end) = do
  let
    criteriaOk = checkCriteria [
                   isSixDigit,
                   hasAdjacentDigits,
                   digitsDontDecrease]
    passwords = [p | p <- [start..end], criteriaOk p]
    passwordCount = length passwords
  return $ show passwordCount


--solution_p2 :: ([Direction Int],[Direction Int]) -> IO String
--solution_p2 = return ""


solution :: IO DailyChallenge
solution = do
  input <- getInput
  --print input
  return $ DailyChallenge (solution_p1 input) Nothing
