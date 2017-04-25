triangles = [(a,b,a^2+b^2) | a <- [1,2..1000], b <- [1,2..1000]]

rightTriangles = [(a,b,c) | (a, b, c) <- triangles, a <= 10, b <= 10, c <= 10, a+b+c==24]

replicate' :: (Num i, Ord i) =>  i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x


maximum' :: (Ord a) => [a] -> a
maximum' lst
  | [] = "Error"
  | [x] = x
  | (h:tl) = max h (maximum' tl)


    
