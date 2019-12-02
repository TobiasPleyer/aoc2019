{-# LANGUAGE FlexibleContexts #-}
module Aoc.Day2
    ( solution
    ) where


import           Control.Monad                (when)
import           Control.Monad.ST             (ST(..), runST)
import qualified Data.Array.IArray            as I
import           Data.Array.MArray            (MArray)
import qualified Data.Array.MArray            as A
import           Data.Array.ST                (STUArray, runSTUArray)
import qualified Data.Array.ST                as A
import qualified Text.Read.Lex                as L
import qualified Text.ParserCombinators.ReadP as P

import Aoc.Common (DailyChallenge(..), isDebug)


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


run1202Program
  :: MArray a Int m
  => [Int]
  -> Int
  -> m (a Int Int)
run1202Program input maxIterations = do
  initialArray <- A.newListArray (0, length input - 1) input
  A.writeArray initialArray 1 12
  A.writeArray initialArray 2 2
  finalArray <- executeProgram initialArray 0 maxIterations
  return finalArray


executeProgram
  :: MArray a Int m
  => a Int Int      -- The input array
  -> Int            -- The index of the current instruction
  -> Int            -- The maximum number of iterations performed (<0 means
                    -- infinitly many)
  -> m (a Int Int)  -- The resulting array after the instruction was executed
executeProgram inputArray instructionIndex maxIterations = do
  if maxIterations == 0
  then return inputArray
  else do
    instruction <- A.readArray inputArray instructionIndex
    newArray <- executeInstruction inputArray instructionIndex maxIterations instruction
    return newArray


executeInstruction
  :: MArray a Int m
  => a Int Int      -- The input array
  -> Int            -- The index of the current instruction
  -> Int            -- The maximum number of iterations performed (<0 means
                    -- infinitly many)
  -> Int            -- The current instruction value
  -> m (a Int Int)  -- The resulting array after the instruction was executed
executeInstruction inputArray instructionIndex maxIterations instruction
  | instruction == 99 = return inputArray
  -- addition shall be performed
  | instruction == 1 = do
      executeBinOp (+) inputArray instructionIndex
      executeProgram inputArray (instructionIndex + 4) (maxIterations - 1)
  -- multiplication shall be performed
  | instruction == 2 = do
      executeBinOp (*) inputArray instructionIndex
      executeProgram inputArray (instructionIndex + 4) (maxIterations - 1)
  -- unknown instruction - this should never happen!
  | otherwise = error "Unknown instruction"


executeBinOp binOp inputArray instructionIndex = do
  indexArg1 <- A.readArray inputArray (instructionIndex + 1)
  indexArg2 <- A.readArray inputArray (instructionIndex + 2)
  indexTarget <- A.readArray inputArray (instructionIndex + 3)
  arg1 <- A.readArray inputArray indexArg1
  arg2 <- A.readArray inputArray indexArg2
  A.writeArray inputArray indexTarget (binOp arg1 arg2)


solution_p1 :: [Int] -> IO String
solution_p1 input = do
  let
    finalArray = runSTUArray $ run1202Program input (-1)
    value = (I.!) finalArray 0 
  when isDebug (print finalArray)
  return $ show value


solution :: IO DailyChallenge
solution = do
  input <- getInput
  return $ DailyChallenge (solution_p1 input) Nothing
