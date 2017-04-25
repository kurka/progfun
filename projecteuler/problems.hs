p001 :: [Int] -> Int
p001 xs = sum [x | x <- xs, x `mod` 3 == 0 || x `mod` 5 == 0]

p001s :: Int -> Int
p001s t = sumDivisibleBy(3)+sumDivisibleBy(5)-sumDivisibleBy(15)
  where sumDivisibleBy n = n*(p*(p+1)) `div` 2
          where p = t `div` n


p002 = sum $ filter even $ takeWhile (< 4000000) fib
  where fib = 0 : 1 : zipWith (+) fib (tail fib)

-- aux functions
-- and and or functions for multiple predicate use
(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = (f a) && (g a)

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.||.) f g a = (f a) || (g a)
