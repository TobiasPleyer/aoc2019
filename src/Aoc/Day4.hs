module Aoc.Day4
    ( solution
    ) where


import Control.Arrow ((***))
import Data.List (break)

import Aoc.Common (DailyChallenge(..), isDebug)


getInput
  :: IO (Int,Int)
getInput = ((read *** (read . tail)) . break (=='-')) <$> readFile "inputs/day4.txt"


--solution_p1 :: ([Direction Int],[Direction Int]) -> IO String
solution_p1 = return ""


--solution_p2 :: ([Direction Int],[Direction Int]) -> IO String
--solution_p2 = return ""


solution :: IO DailyChallenge
solution = do
  input <- getInput
  --print input
  return $ DailyChallenge (solution_p1 ) Nothing
