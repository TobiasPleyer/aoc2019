module Main where


import qualified Chronos as C
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)

import qualified Aoc as Aoc


getSolution
  :: IntMap (IO ())
  -> C.Day  -- date of the start (1st of December 2019)
  -> C.Day  -- today's date
  -> Int    -- the number of the day of interest (1 based, not 0)
  -> IO ()  -- the resulting IO action
getSolution m s t i =
  let
    s' = C.getDay s
    t' = C.getDay t
  in do
    putStr $ "Problem " ++ show i ++ ": "
    if (s'+i-1 > t')
    then putStrLn "Still not unlocked"
    else case IM.lookup i m of
      Just v -> do
        putStrLn ""
        v
      Nothing -> putStrLn "Not solved yet"

startOfAOC2019 = C.Day 58818

main :: IO ()
main = do
  t <- C.today
  putStrLn "Welcome to AOC 2019!"
  putStrLn "Here is the current status:"
  sequence_ [getSolution Aoc.solutions startOfAOC2019 t day | day <- [1..25]]
