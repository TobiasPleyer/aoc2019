module Aoc.Day6 where
--    ( solution
--    ) where


import           Control.Monad                (when)
import qualified System.Console.CmdArgs.Verbosity as V
import qualified Text.Read.Lex                as L
import qualified Text.ParserCombinators.ReadP as P

import Aoc.Common ( DailyChallenge(..)
                  )


type Orbit = (String,String)


getInput
  :: IO [Orbit]
getInput = (map readOrbit . lines) <$> readFile "inputs/day6.txt"
  where readOrbit = fst
                  . last
                  . P.readP_to_S (do
                      object1 <- P.munch1 (/=')')
                      L.expect (L.Punc ")")
                      object2 <- P.look
                      return (object1,object2)
                      )



solution_p1 :: [Orbit] -> IO String
solution_p1 input = do
  V.setVerbosity V.Quiet
  return $ ""


solution :: IO DailyChallenge
solution = do
  input <- getInput
  return $ DailyChallenge (solution_p1 input) Nothing
