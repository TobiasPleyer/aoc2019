module Aoc.Day4
    ( solution
    ) where


import Control.Arrow ((***))
import Data.Digits (digits)
import Data.List (break, group)

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
hasAdjacentDigits n = any ((>=2) . length) $ group (getDigits n)


hasExactlyTwoAdjacentDigits :: Int -> Bool
hasExactlyTwoAdjacentDigits n = any ((==2) . length) $ group (getDigits n)


digitsDontDecrease :: Int -> Bool
digitsDontDecrease n = all (\(a,b) -> a <= b) (zip (init ds) (tail ds))
  where ds = getDigits n


solution_p1 :: (Int,Int) -> IO String
solution_p1 (start,end) = do
  let
    criteriaOk = checkCriteria [
                   isSixDigit,
                   hasAdjacentDigits,
                   digitsDontDecrease]
    passwords = [p | p <- [start..end], criteriaOk p]
    passwordCount = length passwords
  return $ show passwordCount


solution_p2 :: (Int,Int) -> IO String
solution_p2 (start,end) = do
  let
    criteriaOk = checkCriteria [
                   isSixDigit,
                   hasExactlyTwoAdjacentDigits,
                   digitsDontDecrease]
    passwords = [p | p <- [start..end], criteriaOk p]
    passwordCount = length passwords
  return $ show passwordCount


solution :: IO DailyChallenge
solution = do
  input <- getInput
  --print input
  return $ DailyChallenge (solution_p1 input) (Just (solution_p2 input))
