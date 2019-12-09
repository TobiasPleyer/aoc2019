{-# LANGUAGE FlexibleContexts #-}
module Aoc.Common where


import qualified Data.Array.IArray            as I
import           Data.Array.IO                (IOUArray)
import           Data.Array.MArray            (MArray)
import qualified Data.Array.MArray            as A
import           Data.Digits                  (digits)


data DailyChallenge =
  DailyChallenge
  { part1 :: IO String
  , part2 :: Maybe (IO String)
  }


isDebug = False


mkArray :: [Int] -> IO (IOUArray Int Int)
mkArray ints =  A.newListArray (0, length ints - 1) ints


data InputMode
  = Interactive
  | Scripted [Int]


runProgram
  :: InputMode
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
  :: InputMode              -- If we don't want to really interact with the
                            -- program we can feed it input in advance
  -> IOUArray Int Int       -- The input array
  -> Int                    -- The index of the current instruction
  -> Int                    -- The maximum number of iterations performed (<0
                            -- means infinitly many)
  -> IO (IOUArray Int Int)  -- The resulting array after the instruction was executed
executeProgram inputMode inputArray instructionIndex maxIterations = do
  if maxIterations == 0
  then return inputArray
  else do
    instruction <- A.readArray inputArray instructionIndex
    --putStrLn $ "Executing instruction " ++ (show instruction) ++ " at index " ++ (show instructionIndex)
    let
      paramModes = getParameterModes instruction
      getArg n = getArgWithMode inputArray instructionIndex (paramModes !! n) n
    executeInstruction inputMode getArg inputArray instructionIndex maxIterations instruction


executeInstruction
  :: InputMode              -- If we don't want to really interact with the
                            -- program we can feed it input in advance
  -> (Int -> IO Int)        -- The function responsible for getting the arguments
  -> IOUArray Int Int       -- The input array
  -> Int                    -- The index of the current instruction
  -> Int                    -- The maximum number of iterations performed (<0
                            -- means infinitly many)
  -> Int                    -- The current instruction value
  -> IO (IOUArray Int Int)  -- The resulting array after the instruction was executed
executeInstruction inputMode getArg inputArray instructionIndex maxIterations instruction
  | instruction == 99 = do
      --putStrLn "Program finished"
      return inputArray
  -- addition shall be performed
  | instruction == 1 = do
      executeBinOp (+) getArg inputArray instructionIndex
      executeProgram inputMode inputArray (instructionIndex + 4) (maxIterations - 1)
  -- multiplication shall be performed
  | instruction == 2 = do
      executeBinOp (*) getArg inputArray instructionIndex
      executeProgram inputMode inputArray (instructionIndex + 4) (maxIterations - 1)
  | instruction == 3 = do
      indexTarget <- getArg 1
      case inputMode of
        Interactive -> do
          n <- read <$> getLine :: IO Int
          A.writeArray inputArray indexTarget n
          executeProgram inputMode inputArray (instructionIndex + 2) (maxIterations - 1)
        Scripted (n:ns) -> do
          A.writeArray inputArray indexTarget n
          executeProgram (Scripted ns) inputArray (instructionIndex + 2) (maxIterations - 1)
  | instruction == 4 = do
      indexSource <- getArg 1
      value <- A.readArray inputArray indexSource
      putStr $ (show value) ++ " "
      executeProgram inputMode inputArray (instructionIndex + 2) (maxIterations - 1)
  -- unknown instruction - this should never happen!
  | otherwise = error $ "Unknown instruction: " ++ show instruction


getParameterModes :: Int -> [Int]
getParameterModes n = 
  if n < 10 then repeat 0 else ((++ (repeat 0)) . drop 2 . reverse . digits 10) n


getArgWithMode arr base mode index = do
  if mode == 0 then do  -- mode 0 == position mode
    --putStrLn $ "Getting arg " ++ (show index) ++ " in position mode"
    indexArg <- A.readArray arr (base+index)
    A.readArray arr indexArg
  else do  -- immediate mode
    --putStrLn $ "Getting arg " ++ (show index) ++ " in immediate mode"
    A.readArray arr (base+index)


executeBinOp binOp getArg inputArray instructionIndex = do
  arg1 <- getArg 1
  arg2 <- getArg 2
  indexTarget <- A.readArray inputArray (instructionIndex+3)
  A.writeArray inputArray indexTarget (binOp arg1 arg2)
