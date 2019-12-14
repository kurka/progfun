module Day3 where

import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map as M


data Point = Point Int Int deriving (Eq, Ord, Show)

addPoint :: Point -> Point -> Point
addPoint (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

parsePath :: String -> [Point]
parsePath = scanl1 addPoint . concatMap parseCommands . splitOn ","
  where -- add one point per step of moviments
    parseCommands (directions:num) = replicate (read num) $ case directions of
      'U' -> Point 0 1
      'D' -> Point 0 (-1)
      'R' -> Point 1 0
      'L' -> Point (-1) 0
      _ -> error "non defined instruction!"
    parseCommands _ = []

-- calculate manhattan distance to origin
manhDist :: Point -> Int
manhDist (Point x y) = abs x + abs y

solveA :: [Point] -> [Point] -> Int
solveA pathA pathB = minimum $ map manhDist intersections
  where
    intersections = S.toList $ S.intersection (S.fromList pathA) (S.fromList pathB)
    

solveB :: [Point] -> [Point] -> Int
solveB pathA pathB = minimum intersections 
  where
    mPath = M.fromListWith min . flip zip [1 ..] -- create map with Point, time
    mPathA = mPath pathA
    mPathB = mPath pathB
    intersections = M.elems $ M.intersectionWith (+) mPathA mPathB


main :: IO()
main = do
  text <- readFile "3.in"
  let paths = map parsePath $ lines text
  let pathA = head paths
  let pathB = paths !! 1

  print $ solveA pathA pathB
  print $ solveB pathA pathB

