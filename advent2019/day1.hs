module Day1 where

readInt :: String -> Int
readInt = read

fuel :: Int -> Int
fuel x = max (div x 3 - 2) 0

solveA :: [Int] -> Int
solveA = sum . map fuel

solveB :: [Int] -> Int
solveB = sum . map rfuel
  where rfuel x
          | x > 0 = fuel x + rfuel (fuel x)
          | otherwise = 0



main = do
  text <- readFile "1.in"
  let ms = fmap readInt $ lines text

  print $ solveA ms
  print $ solveB ms
