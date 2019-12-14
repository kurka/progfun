module Day4 where

import Data.Char (digitToInt)
import Data.List (group)
import Control.Monad

toList :: Int -> [Int]
toList = map digitToInt . show


toInt :: [Int] -> Int
toInt xs = read $ concatMap show xs


-- combine different boolean functions with 'and'
(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) = liftM2 (&&)

checkCandidate :: [Int] -> Bool
checkCandidate = (check1 .&&. check2 .&&. check3)
  where
    -- check if length == 6
    check1 = (== 6) . length
    -- check if has adjacent items
    check2 (x1: x2: _) | x1==x2 = True
    check2 (_: xs) = check2 xs
    check2 [] = False
    -- check if monotonic crescent in digits
    check3 (x1: x2: xs)
      | x2 >= x1 = check3 (x2:xs)
      | otherwise = False
    check3 _ = True


checkCandidatePlus :: [Int] -> Bool
checkCandidatePlus = checkExtra .&&. checkCandidate
  where
    checkExtra :: [Int] -> Bool
    checkExtra = elem 2 . map length . group

solveA :: Int -> Int -> Int
solveA rMin rMax = length [x | x <- [rMin..rMax], checkCandidate $ toList x]
-- (iteration could be smarter (e.g. monotonic, always generate pairs), but life
-- is short for these optimisations)

solveB :: Int -> Int -> Int
solveB rMin rMax = length [x | x <- [rMin..rMax], checkCandidatePlus $ toList x]
-- (iteration could be smarter (e.g. monotonic, always generate pairs), but life
-- is short for these optimisations)

main :: IO()
main = do
  let rMin = 123257
  let rMax = 647015
  print $ solveA rMin rMax
  print $ solveB rMin rMax

