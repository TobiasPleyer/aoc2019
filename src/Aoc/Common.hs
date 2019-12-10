{-# LANGUAGE FlexibleContexts #-}
module Aoc.Common where


import           Control.Monad                (when)
import qualified Data.Array.IArray            as I
import           Data.Array.IO                (IOUArray)
import           Data.Array.MArray            (MArray)
import qualified Data.Array.MArray            as A
import qualified Data.Array.Unboxed           as U
import           Data.Digits                  (digits)
import qualified System.Console.CmdArgs.Verbosity as V


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
  putStrLn ""
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
    isLoud <- V.isLoud
    when isLoud $ do
      arr <- A.freeze inputArray :: IO (U.UArray Int Int)
      print arr
    let
      opcode = getOpcode instruction
      paramModes = getParameterModes instruction
      getArg n = getArgWithMode inputArray instructionIndex (paramModes !! n) n
    when isLoud (putStrLn $ "Executing " ++ (show instruction) ++ " at index " ++ (show instructionIndex))
    executeInstruction inputMode getArg inputArray instructionIndex maxIterations opcode


executeInstruction
  :: InputMode              -- If we don't want to really interact with the
                            -- program we can feed it input in advance
  -> (Int -> IO Int)        -- The function responsible for getting the arguments
  -> IOUArray Int Int       -- The input array
  -> Int                    -- The index of the current instruction
  -> Int                    -- The maximum number of iterations performed (<0
                            -- means infinitly many)
  -> Int                    -- The current opcode value
  -> IO (IOUArray Int Int)  -- The resulting array after the instruction was executed
executeInstruction inputMode getArg inputArray instructionIndex maxIterations opcode
  | opcode == 99 = do
      isLoud <- V.isLoud
      when isLoud (putStrLn "Program finished")
      return inputArray
  -- addition shall be performed
  | opcode == 1 = do
      executeBinOp (+) getArg inputArray instructionIndex
      executeProgram inputMode inputArray (instructionIndex + 4) (maxIterations - 1)
  -- multiplication shall be performed
  | opcode == 2 = do
      executeBinOp (*) getArg inputArray instructionIndex
      executeProgram inputMode inputArray (instructionIndex + 4) (maxIterations - 1)
  -- read number
  | opcode == 3 = do
      indexTarget <- A.readArray inputArray (instructionIndex+1)
      case inputMode of
        Interactive -> do
          n <- read <$> getLine :: IO Int
          A.writeArray inputArray indexTarget n
          executeProgram inputMode inputArray (instructionIndex + 2) (maxIterations - 1)
        Scripted (n:ns) -> do
          A.writeArray inputArray indexTarget n
          executeProgram (Scripted ns) inputArray (instructionIndex + 2) (maxIterations - 1)
  -- print number
  | opcode == 4 = do
      value <- getArg 1
      putStr $ (show value) ++ " "
      executeProgram inputMode inputArray (instructionIndex + 2) (maxIterations - 1)
  -- jump-if-true
  | opcode == 5 = do
      value <- getArg 1
      if value /= 0
      then do
        newIndex <- getArg 2
        executeProgram inputMode inputArray newIndex (maxIterations - 1)
      else
        executeProgram inputMode inputArray (instructionIndex + 3) (maxIterations - 1)
  -- jump-if-false
  | opcode == 6 = do
      value <- getArg 1
      if value == 0
      then do
        newIndex <- getArg 2
        executeProgram inputMode inputArray newIndex (maxIterations - 1)
      else
        executeProgram inputMode inputArray (instructionIndex + 3) (maxIterations - 1)
  -- less than
  | opcode == 7 = do
      v1 <- getArg 1
      v2 <- getArg 2
      let r = if v1 < v2 then 1 else 0
      indexTarget <- A.readArray inputArray (instructionIndex+3)
      A.writeArray inputArray indexTarget r
      executeProgram inputMode inputArray (instructionIndex + 4) (maxIterations - 1)
  -- equals
  | opcode == 8 = do
      v1 <- getArg 1
      v2 <- getArg 2
      let r = if v1 == v2 then 1 else 0
      indexTarget <- A.readArray inputArray (instructionIndex+3)
      A.writeArray inputArray indexTarget r
      executeProgram inputMode inputArray (instructionIndex + 4) (maxIterations - 1)
  -- unknown instruction - this should never happen!
  | otherwise = error $ "Unknown opcode: " ++ show opcode


getOpcode :: Int -> Int
getOpcode n = n `mod` 100


getParameterModes :: Int -> [Int]
getParameterModes n = 
  -- the zeroth (0th) parameter is always the instruction itself and thus must
  -- be in immediate mode
  1 : (if n < 10 then repeat 0 else ((++ (repeat 0)) . drop 2 . reverse . digits 10) n)


getArgWithMode arr base mode index = do
  isLoud <- V.isLoud
  if mode == 0 then do  -- mode 0 == position mode
    when isLoud (putStrLn $ "Getting arg " ++ (show index) ++ " in position mode")
    indexArg <- A.readArray arr (base+index)
    A.readArray arr indexArg
  else do  -- immediate mode
    when isLoud (putStrLn $ "Getting arg " ++ (show index) ++ " in immediate mode")
    A.readArray arr (base+index)


executeBinOp binOp getArg inputArray instructionIndex = do
  arg1 <- getArg 1
  arg2 <- getArg 2
  indexTarget <- A.readArray inputArray (instructionIndex+3)
  A.writeArray inputArray indexTarget (binOp arg1 arg2)
