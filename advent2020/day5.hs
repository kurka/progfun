-- | day 5

module Day5 where

import Data.List
import Data.List.Split


binarySearch :: Int -> Int -> [Char] -> Int
binarySearch low high (x:xs)
  | x `elem` ['F', 'L'] = binarySearch low (low+increment-1) xs
  | x `elem` ['B', 'R'] = binarySearch (low+increment) high xs
  | otherwise = error "Undefined char in string"
  where
    increment = (high-low+1) `div` 2
binarySearch low high []
  | low == high = low
  | otherwise = error "There is something weird in my logic"


parseBPass :: String -> Int
parseBPass bPass = (binarySearch 0 127 rows) * 8 +  (binarySearch 0 7 columns)
  where
    [rows, columns] = splitPlaces [7,3] bPass


solveA :: [String] -> Int
solveA = maximum . map parseBPass

solveB :: [String] -> Int
-- solveB bPasses = filter different $ zip [1..] sortedBPasses
solveB bPasses = fst . head . filter different $ zip [(head sortedBPasses)..] sortedBPasses
  where
    sortedBPasses = sort $ map parseBPass bPasses
    different (a,b) = a /= b

main :: IO()
main = do
  text <- readFile "5.in"

  print . solveA $ lines text
  print . solveB $ lines text
