
p004 = maximum [(a*b, a, b) | a <- [999,998..100], b <- [999,998..a], palindrome (a*b)]
  where palindrome x = (show x) == (reverse $ show x)
