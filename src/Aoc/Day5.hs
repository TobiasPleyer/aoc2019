{-# LANGUAGE FlexibleContexts #-}
module Aoc.Day5
    ( solution
    ) where


import           Control.Monad                (when)
import qualified Data.Array.MArray            as A
import           Data.Maybe                   (fromJust)
import qualified System.Console.CmdArgs.Verbosity as V
import qualified Text.Read.Lex                as L
import qualified Text.ParserCombinators.ReadP as P

import Aoc.Common ( DailyChallenge(..)
                  , isDebug
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


solution :: IO DailyChallenge
solution = do
  input <- getInput
  return $ DailyChallenge (solution_p1 input) Nothing
