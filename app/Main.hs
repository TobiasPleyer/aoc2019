module Main where


import qualified Chronos as C
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)

import           Aoc.Common (DailyChallenge(..))
import qualified Aoc as Aoc


data SelectionMode
  = Today
  | Some [Int]
  | All


printSolution
  :: IntMap DailyChallenge
  -> Int    -- date of the start (1st of December 2019)
  -> Int    -- today's date
  -> Int    -- the number of the day of interest (1 based, not 0)
  -> IO ()  -- the resulting IO action
printSolution m s t i = do
    putStr $ "Day " ++ show i ++ ": "
    if (s+i-1 > t)
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


startOfAOC2019 = 58818  -- 1st of December 2019
selectionMode = Today
--selectionMode = Some [3,4]
--selectionMode = All


main :: IO ()
main = do
  t <- C.getDay <$> C.today
  solutions <- Aoc.getSolutions
  putStrLn "Welcome to AOC 2019!"
  let days = case selectionMode of
                    Today -> [t - startOfAOC2019 + 1]
                    Some days -> filter (\d -> (d >= 1) && (d <= 25)) days
                    All -> [1..25]
  putStrLn "Here is the current status:"
  sequence_ [printSolution solutions startOfAOC2019 t day | day <- days]
