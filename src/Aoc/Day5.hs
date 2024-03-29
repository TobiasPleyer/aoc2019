{-# LANGUAGE FlexibleContexts #-}
module Aoc.Day5
    ( solution
    ) where


import           Control.Monad                (when)
import qualified Data.Array.MArray            as A
import qualified System.Console.CmdArgs.Verbosity as V

import Aoc.Common ( DailyChallenge(..)
                  , executeProgram
                  , InputMode(..)
                  , mkArray
                  )


getInput
  :: IO [Int]
getInput = (read . makeList) <$> readFile "inputs/day5.txt"
  where makeList s = "[" ++ s ++ "]"


solution_p1 :: [Int] -> IO String
solution_p1 input = do
  V.setVerbosity V.Quiet
  arr <- mkArray input
  executeProgram (Scripted [1]) arr 0 (-1)
  putStrLn ""
  r <- A.readArray arr 0
  return $ show r


solution_p2 :: [Int] -> IO String
solution_p2 input = do
  V.setVerbosity V.Quiet
  arr <- mkArray input
  executeProgram (Scripted [5]) arr 0 (-1)
  putStrLn ""
  r <- A.readArray arr 0
  return $ show r


solution :: IO DailyChallenge
solution = do
  input <- getInput
  return $ DailyChallenge (solution_p1 input) (Just (solution_p2 input))
