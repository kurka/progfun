-- | Day 6

module Day6 where

import qualified Data.Set as S
import Data.List.Split (splitOn)

solve :: ([S.Set Char] -> S.Set Char) -> [String] -> Int
solve f = sum . map (S.size . f . map S.fromList . lines)

solveA :: [String] -> Int
solveA = solve S.unions

solveB :: [String] -> Int
solveB = solve (foldl1 S.intersection)

main :: IO()
main = do
  text <- readFile "6.in"
  let groups = splitOn "\n\n" text
  print $ solveA groups
  print $ solveB groups
