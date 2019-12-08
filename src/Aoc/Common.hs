{-# LANGUAGE FlexibleContexts #-}
module Aoc.Common where


import qualified Data.Array.IArray            as I
import           Data.Array.IO                (IOUArray)
import           Data.Array.MArray            (MArray)
import qualified Data.Array.MArray            as A


data DailyChallenge =
  DailyChallenge
  { part1 :: IO String
  , part2 :: Maybe (IO String)
  }


isDebug = False


mkArray :: [Int] -> IO (IOUArray Int Int)
mkArray ints =  A.newListArray (0, length ints - 1) ints


data ExecutionMode
  = Interactive
  | Scripted [Int]


runProgram
  :: ExecutionMode
  -> [Int]
  -> Int
  -> Int
  -> Int
  -> IO Int
runProgram mode input noun verb maxIterations = do
  arr <- mkArray input
  A.writeArray arr 1 noun
  A.writeArray arr 2 verb
  executeProgram mode arr 0 maxIterations
  A.readArray arr 0


executeProgram
  :: ExecutionMode          -- If we don't want to really interact with the
                            -- program we can feed it input in advance
  -> IOUArray Int Int       -- The input array
  -> Int                    -- The index of the current instruction
  -> Int                    -- The maximum number of iterations performed (<0
                            -- means infinitly many)
  -> IO (IOUArray Int Int)  -- The resulting array after the instruction was executed
executeProgram mode inputArray instructionIndex maxIterations = do
  if maxIterations == 0
  then return inputArray
  else do
    instruction <- A.readArray inputArray instructionIndex
    newArray <- executeInstruction mode inputArray instructionIndex maxIterations instruction
    return newArray


executeInstruction
  :: ExecutionMode          -- If we don't want to really interact with the
                            -- program we can feed it input in advance
  -> IOUArray Int Int       -- The input array
  -> Int                    -- The index of the current instruction
  -> Int                    -- The maximum number of iterations performed (<0
                            -- means infinitly many)
  -> Int                    -- The current instruction value
  -> IO (IOUArray Int Int)  -- The resulting array after the instruction was executed
executeInstruction mode inputArray instructionIndex maxIterations instruction
  | instruction == 99 = return inputArray
  -- addition shall be performed
  | instruction == 1 = do
      executeBinOp (+) inputArray instructionIndex
      executeProgram mode inputArray (instructionIndex + 4) (maxIterations - 1)
  -- multiplication shall be performed
  | instruction == 2 = do
      executeBinOp (*) inputArray instructionIndex
      executeProgram mode inputArray (instructionIndex + 4) (maxIterations - 1)
  | instruction == 3 = do
      indexTarget <- A.readArray inputArray (instructionIndex + 1)
      case mode of
        Interactive -> do
          n <- read <$> getLine :: IO Int
          A.writeArray inputArray indexTarget n
          executeProgram mode inputArray (instructionIndex + 2) (maxIterations - 1)
        Scripted (n:ns) -> do
          A.writeArray inputArray indexTarget n
          executeProgram (Scripted ns) inputArray (instructionIndex + 2) (maxIterations - 1)
  | instruction == 4 = do
      indexSource <- A.readArray inputArray (instructionIndex + 1)
      value <- A.readArray inputArray indexSource
      putStr $ (show value) ++ " "
      executeProgram mode inputArray (instructionIndex + 2) (maxIterations - 1)
  -- unknown instruction - this should never happen!
  | otherwise = error $ "Unknown instruction: " ++ show instruction


executeBinOp binOp inputArray instructionIndex = do
  indexArg1 <- A.readArray inputArray (instructionIndex + 1)
  indexArg2 <- A.readArray inputArray (instructionIndex + 2)
  indexTarget <- A.readArray inputArray (instructionIndex + 3)
  arg1 <- A.readArray inputArray indexArg1
  arg2 <- A.readArray inputArray indexArg2
  A.writeArray inputArray indexTarget (binOp arg1 arg2)
