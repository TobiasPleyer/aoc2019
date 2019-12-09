{-# LANGUAGE FlexibleContexts #-}
module Aoc.Day2
    ( solution
    ) where


import           Control.Monad                (when)
import           Data.Maybe                   (fromJust)
import qualified Text.Read.Lex                as L
import qualified Text.ParserCombinators.ReadP as P

import Aoc.Common (DailyChallenge(..), isDebug, runProgram, InputMode(..))


-- The simple shortcut: Make the input string a valid list by enclosing it in
-- brackets and then use the fact that `[a]` is an instance of `Read` if `a`
-- is.
getInput
  :: IO [Int]
getInput = (read . makeList) <$> readFile "inputs/day2.txt"
  where makeList s = "[" ++ s ++ "]"


-- The hard way: Actually define the parser using the primitives from
-- `Text.Read.Lex` and `Text.ParserCombinators.ReadP`
getInput2
  :: IO [Int]
getInput2 = readInts <$> readFile "inputs/day2.txt"
  where readInts = fst   -- Only take the value, ignore the left-over string
                 . last  -- We are interest in the maximum match, i.e. the
                         -- whole list
                 -- The parser: Parses integers separated by commas (whitespace
                 -- is ignored)
                 . P.readP_to_S (L.readDecP `P.sepBy` (L.expect (L.Punc ",")))


solution_p1 :: [Int] -> IO String
solution_p1 input = do
  r <- runProgram (Scripted []) input 12 2 (-1)
  return $ show r


solution_p2 :: [Int] -> IO String
solution_p2 input = do
  let
    nouns = [0..99]
    verbs = [0..99]
    mkValue = \(n,v) -> 100*n + v
    -- We can construct all possible combinations, laziness will ensure that we
    -- just calculate as much as necessary
    runProg = \n v -> do
      r <- runProgram (Scripted []) input n v (-1)
      return (r,(n,v))
  results <- sequence [runProg n v | n <- nouns, v <- verbs]
  let
    result = lookup 19690720 results
  return $ show $ mkValue $ fromJust result


solution :: IO DailyChallenge
solution = do
  input <- getInput
  return $ DailyChallenge (solution_p1 input) (Just (solution_p2 input))
