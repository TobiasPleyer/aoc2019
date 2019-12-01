module Aoc (
  solutions
  ) where

import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)

import qualified Aoc.Day1 as Day1

solutions :: IntMap (IO ())
solutions = IM.fromList [
  (1, Day1.solution)
  ]
