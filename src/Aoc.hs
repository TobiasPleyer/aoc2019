module Aoc (
  getSolutions
  ) where

import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)

import           Aoc.Common (DailyChallenge(..))
import qualified Aoc.Day1 as Day1
import qualified Aoc.Day2 as Day2
import qualified Aoc.Day3 as Day3

getSolutions :: IO (IntMap DailyChallenge)
getSolutions = do
  s1 <- Day1.solution
  s2 <- Day2.solution
  s3 <- Day3.solution
  return $ IM.fromList $ zip [1..] [s1,s2,s3]
