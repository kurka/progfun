p001 :: [Int] -> Int
p001 xs = sum [x | x <- xs, x `mod` 3 == 0 || x `mod` 5 == 0]

p001s :: Int -> Int
p001s t = sumDivisibleBy(3)+sumDivisibleBy(5)-sumDivisibleBy(15)
  where sumDivisibleBy n = n*(p*(p+1)) `div` 2
          where p = t `div` n

p002 = sum $ filter even $ takeWhile (< 4000000) fib
  where fib = 0 : 1 : zipWith (+) fib (tail fib)


p004 = maximum [(a*b, a, b) | a <- [999,998..100], b <- [999,998..a], palindrome (a*b)]
  where palindrome x = (show x) == (reverse $ show x)

-- p005 = head [x | x <- [2520..], all ((==0) . (mod x)) [2..20]]
p005 = 16*9*5*7*11*13*17*19


p006 = (squareOfSum 100) - (sumOfSquares 100)
  where squareOfSum n = (n*(n+1))^2 `div` 4
        sumOfSquares n = (n*(n+1)*(2*n+1)) `div` 6

p006s n = (3*n^4 + 2*n^3 - 3*n^2 - 2*n) `div` 12

-- aux functions
-- and and or functions for multiple predicate use
(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = (f a) && (g a)

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.||.) f g a = (f a) || (g a)
