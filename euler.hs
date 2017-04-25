
p004 = maximum [(a*b, a, b) | a <- [999,998..100], b <- [999,998..a], palindrome (a*b)]
  where palindrome x = (show x) == (reverse $ show x)


-- p005 = head [x | x <- [2520..], all ((==0) . (mod x)) [2..20]]
p005 = 16*9*5*7*11*13*17*19
