module Aoc.Day3 where
--    ( solution
--    ) where

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
  L n -> take n (tail $ iterate goLeft start)
  R n -> take n (tail $ iterate goRight start)
  U n -> take n (tail $ iterate goUp start)
  D n -> take n (tail $ iterate goDown start)

center :: Point Int
center = (0,0)


manhatten :: Point Int -> Int
manhatten (x,y) = (abs x) + (abs y)


mkPointsSet :: Point Int -> [Direction Int] -> Set (Point Int)
mkPointsSet = helper S.empty
  where
    helper points start [] = points
    helper points start (d:ds) =
      let
        newPoints = followDirection start d
        endPoint = last newPoints  -- the end will be the next start
      in helper (S.union points (S.fromList newPoints)) endPoint ds


mkPointsList :: Point Int -> [Direction Int] -> [Point Int]
mkPointsList = helper []
  where
    helper points start [] = points
    helper points start (d:ds) =
      let
        newPoints = followDirection start d
        endPoint = last newPoints  -- the end will be the next start
      in helper (points ++ newPoints) endPoint ds


findCommonPoints :: Set (Point Int) -> Set (Point Int) -> Set (Point Int)
findCommonPoints = S.intersection


findClosestPoint :: Set (Point Int) -> Point Int
findClosestPoint points = S.foldr' minF (S.elemAt 0 points) points
  where minF a b = if (manhatten a) < (manhatten b) then a else b


findSmallestDist :: Set (Point Int) -> Int
findSmallestDist = manhatten . findClosestPoint


countStepsUntilPoint :: Point Int -> [Point Int] -> [Point Int] -> Int
countStepsUntilPoint target wire1Points wire2Points =
  let
    wire1Steps = takeWhile (/= target) wire1Points
    wire2Steps = takeWhile (/= target) wire2Points
  -- +2 because takeWhile "forgets" the last step for each wire
  in (length wire1Steps) + (length wire2Steps) + 2  


solution_p1 :: ([Direction Int],[Direction Int]) -> IO String
solution_p1 (wire1Directions,wire2Directions) = do
  let
    wire1Points = mkPointsSet center wire1Directions
    wire2Points = mkPointsSet center wire2Directions
    minDist = findSmallestDist $ findCommonPoints wire1Points wire2Points
  return $ show minDist


solution_p2 :: ([Direction Int],[Direction Int]) -> IO String
solution_p2 (wire1Directions,wire2Directions) = do
  let
    wire1Points = mkPointsList center wire1Directions
    wire2Points = mkPointsList center wire2Directions
    commonPoints = findCommonPoints (S.fromList wire1Points) (S.fromList wire2Points)
    wire1Steps = takeWhile (flip S.notMember commonPoints) wire1Points
    wire2Steps = takeWhile (flip S.notMember commonPoints) wire2Points
  return $ show (minimum [countStepsUntilPoint t wire1Points wire2Points | t <- (S.toList commonPoints)]) 
  
testInput1 = [ [R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51],
               [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7] ]
testInput2 = [ [R 8, U 5, L 5, D 3],
               [U 7, R 6, D 4, L 4] ]

solution :: IO DailyChallenge
solution = do
  input <- getInput
  when isDebug $ print input
  let
    wire1 = head input
    wire2 = head $ tail input
  return $ DailyChallenge (solution_p1 (wire1,wire2)) (Just (solution_p2 (wire1,wire2)))
