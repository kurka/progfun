-- | Day 3 AOC 2020

module Day3 where

isTree :: Char -> Int
isTree '#' = 1
isTree '.' = 0
isTree _ = error "unexpected char in input!"

slope :: Int -> Int -> Int -> [String] -> Int
slope right down pos (line: nextLines) = isTree (line !! wraped_pos)
  + slope right down (pos+right) (drop (down-1) nextLines)
  where
    wraped_pos = mod pos (length line)
slope _ _ _ []= 0

solveA :: [String] -> Int
solveA = slope 3 1 0


solveB :: [String] -> Int
solveB text_lines = product $ map (\(r, d) -> slope r d 0 text_lines) paths
  where
    paths = [(1,1), (3,1), (5,1), (7,1), (1,2)]
main :: IO()
main = do
  text <- readFile "3.in"

  print . solveA $ lines text
  print . solveB $ lines text
