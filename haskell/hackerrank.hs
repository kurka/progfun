main::IO ()
main = do
  [s, t] <- getInts
  [a, b] <- getInts
  [m, n] <- getInts
  apple <- getInts
  orange <- getInts
  print $ sfruits s t a apple
  print $ sfruits s t b orange


sfruits :: Int -> Int -> Int -> [Int] -> Int
sfruits s t p = length . filter ((>=s) .&&. (<=t)) . map (+p)

getInts::IO [Int]
getInts = fmap ((map read).words) getLine


(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = (f a) && (g a)


findSubstrings :: [String] -> [String] -> [String]
findSubstrings words parts =
  -- sort parts
  -- for each word, return the best match or nothing
  map bestMatch words
  where bestMatch = 
