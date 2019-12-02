module Main where


import qualified Chronos as C
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)

import           Aoc.Common (DailyChallenge(..))
import qualified Aoc as Aoc


printSolution
  :: IntMap DailyChallenge
  -> C.Day  -- date of the start (1st of December 2019)
  -> C.Day  -- today's date
  -> Int    -- the number of the day of interest (1 based, not 0)
  -> IO ()  -- the resulting IO action
printSolution m s t i =
  let
    s' = C.getDay s
    t' = C.getDay t
  in do
    putStr $ "Day " ++ show i ++ ": "
    if (s'+i-1 > t')
    then putStrLn "Still not unlocked"
    else case IM.lookup i m of
      Just (DailyChallenge p1 mp2) -> do
        putStrLn ""
        v1 <- p1
        putStrLn $ "  Part 1: " ++ v1
        v2 <- case mp2 of
                Just p2 -> p2
                Nothing -> return "Not solved yet"
        putStrLn $ "  Part 2: " ++ v2
      Nothing -> putStrLn "Not solved yet"


startOfAOC2019 = C.Day 58818  -- 1st of December 2019


main :: IO ()
main = do
  t <- C.today
  solutions <- Aoc.getSolutions
  putStrLn "Welcome to AOC 2019!"
  putStrLn "Here is the current status:"
  sequence_ [printSolution solutions startOfAOC2019 t day | day <- [1..25]]
