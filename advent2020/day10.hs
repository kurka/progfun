-- | day 10

module Day10 where

import Data.List
import qualified Data.IntMap.Strict as M

solveA :: [Int] -> Int -> Int -> Int
solveA (a:b:c) threes ones
  | b-a == 3 = solveA (b:c) (threes+1) ones
  | b-a == 1 = solveA (b:c) threes (ones+1)
  | otherwise = solveA (b:c) threes ones
solveA _ threes ones = (threes+1) * ones

solveB :: M.IntMap Int -> [Int] -> Int
solveB preComputedPaths [] = preComputedPaths M.! 0
solveB preComputedPaths (x:xs) = solveB preComputedPaths' xs
  where
    preComputedPaths' = M.insert x (lookPath 1 + lookPath 2 + lookPath 3) preComputedPaths
    lookPath p = M.findWithDefault 0 (x+p) preComputedPaths

main :: IO()
main = do
  text <- readFile "10.in"
  let ms = read <$> lines text
  let sMs = 0:sort ms
  print $ solveA sMs 0 0
  let rsMs@(maxMs:_) = reverse sMs
  print $ solveB (M.singleton (maxMs+3) 1) rsMs
