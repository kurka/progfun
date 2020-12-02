-- | day 1 of advent of code 2020

module Day1 where

import Data.List

readInt :: String -> Int
readInt = read


-- find a pair that adds to t, return its product
findPairProduct :: Int -> [Int] -> [Int]
findPairProduct t = repeated . map complementProduct
  where
    complementProduct x = x * (t-x)
    repeated = map head . filter ((>1) . length) . group . sort


-- O(nlogn) solution
solveA :: [Int] -> Int
solveA xs = case findPairProduct 2020 xs of
  [y] -> y
  otherwise -> error "no match or multiple matches"


-- O(n^2logn) solution
solveB :: [Int] -> Int
solveB (x:xs) = case findPairProduct (2020-x) xs of
  [] -> solveB xs
  y -> x * head y
solveB [] = error "list not found"


main = do
  text <- readFile "1.in"
  let ms = readInt <$> lines text

  print $ solveA ms
  print $ solveB ms
