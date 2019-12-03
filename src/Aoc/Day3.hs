module Aoc.Day3
    ( solution
    ) where

import           Control.Monad                (when)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Text.Read                       as R
import qualified Text.Read.Lex                   as L
import qualified Text.ParserCombinators.ReadPrec as RP

import Aoc.Common (DailyChallenge(..), isDebug)


data Direction a
  = L a
  | R a
  | U a
  | D a
  deriving (Show, Ord, Eq)


instance (Eq a, Num a, Read a) => Read (Direction a) where
  readPrec = do
    dir <- RP.lift L.lexChar
    num <- RP.lift L.readDecP
    return $ mkDirection dir num
    where mkDirection d n
            | d == 'L' = L n
            | d == 'R' = R n
            | d == 'U' = U n
            | d == 'D' = D n

  readListPrec = R.readListPrecDefault


getInput
  :: IO [[Direction Int]]
getInput = (map (read . makeList) . lines) <$> readFile "inputs/day3.txt"
  where
    makeList s = "[" ++ s ++ "]"


type Point a = (a,a)


goLeft :: Point Int -> Point Int
goLeft (x,y) = (x-1,y)


goRight :: Point Int -> Point Int
goRight (x,y) = (x+1,y)


goUp :: Point Int -> Point Int
goUp (x,y) = (x,y+1)


goDown :: Point Int -> Point Int
goDown (x,y) = (x,y-1)


followDirection :: Point Int -> Direction Int -> [Point Int]
followDirection start dir = case dir of
  L n -> take (n+1) (iterate goLeft start)
  R n -> take (n+1) (iterate goRight start)
  U n -> take (n+1) (iterate goUp start)
  D n -> take (n+1) (iterate goDown start)

center :: Point Int
center = (0,0)


manhatten :: Point Int -> Int
manhatten (x,y) = (abs x) + (abs y)


findPoints :: Point Int -> [Direction Int] -> Set (Point Int)
findPoints = helper S.empty
  where
    helper points start [] = points
    helper points start (d:ds) =
      let
        linePoints = followDirection start d
        newPoints = tail linePoints
        endPoint = last linePoints  -- the end will be the next start
      in helper (S.union points (S.fromList newPoints)) endPoint ds


findCommonPoints :: Set (Point Int) -> Set (Point Int) -> Set (Point Int)
findCommonPoints = S.intersection


findClosestPoint :: Set (Point Int) -> Point Int
findClosestPoint points = S.foldr' minF (S.elemAt 0 points) points
  where minF a b = if (manhatten a) < (manhatten b) then a else b


findSmallestDist :: Set (Point Int) -> Int
findSmallestDist = manhatten . findClosestPoint


solution_p1 :: ([Direction Int],[Direction Int]) -> IO String
solution_p1 (wire1Directions,wire2Directions) = do
  let
    wire1Points = findPoints center wire1Directions
    wire2Points = findPoints center wire2Directions
    minDist = findSmallestDist $ findCommonPoints wire1Points wire2Points
  return $ show minDist

--solution_p2 :: [Int] -> IO String
solution_p2 input = return "N/A"


solution :: IO DailyChallenge
solution = do
  input <- getInput
  when isDebug $ print input
  let
    wire1 = head input
    wire2 = head $ tail input
  return $ DailyChallenge (solution_p1 (wire1,wire2)) Nothing
