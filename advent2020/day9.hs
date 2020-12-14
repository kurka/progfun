-- | Day 9 - chance to redeem day's 1 solution

module Day9 where

import Data.IntSet
import qualified Data.IntSet as S

readInt :: String -> Int
readInt = read


checkRule :: IntSet -> Int -> Bool
checkRule preamblee x = (> 0) . S.size $ S.filter isFactorOfX preamblee
  where
    -- Note: by using sets, we are assuming that numbers will never be repeated
    -- in the preamble, which is not necessarily true for general case
    isFactorOfX :: Int -> Bool
    isFactorOfX p = (2*p /= x) && ((x-p) `member` preamblee)


solveA :: [Int] -> [Int] -> Int
solveA preamble@(_:ps) (x:xs) = case checkRule (S.fromList preamble) x of
  True -> solveA (ps++[x]) xs
  False -> x
solveA _ [] = error "Reached the end of the list"


solveB :: Int -> Int -> [Int] -> [Int] -> Int
solveB target partialSum contiguous (x:xs) = case compare partialSum target of
  EQ -> minimum contiguous + maximum contiguous -- if found, just return it
  LT -> solveB target (partialSum + x) (contiguous++[x]) xs -- if more continuous numbers can still be added
  GT -> solveB target (partialSum - head contiguous) (tail contiguous) (x:xs) -- if adding a new number causes an overflow
solveB t ps c []
  | t == ps = minimum c + maximum c
  | otherwise = error "Reached the end of the list"


main :: IO()
main = do
  text <- readFile "9.in"
  let ms = readInt <$> lines text

  print $ solveA `uncurry` splitAt 25 ms
  let target = 466456641
  print $ solveB target 0 [] ms
