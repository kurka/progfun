-- | Day 12

module Day12 where


navigate :: (Int, Int, Char) -> (Char, Int) -> (Int, Int, Char)
navigate (ewPos, nsPos, face) (inst, n) = case inst of
  'N' -> (ewPos, nsPos+n, face)
  'S' -> (ewPos, nsPos-n, face)
  'E' -> (ewPos+n, nsPos, face)
  'W' -> (ewPos-n, nsPos, face)
  'L' -> (ewPos, nsPos, turn n)
  'R' -> (ewPos, nsPos, turn (-n))
  'F' -> navigate (ewPos, nsPos, face) (face, n)

  where
    turn x = angle ((dir2angle face + x) `mod` 360)
    angle 0 = 'E'
    angle 90 = 'N'
    angle 180 = 'W'
    angle 270 = 'S'
    angle _ = error "undefined angle!"
    dir2angle 'E' = 0
    dir2angle 'N' = 90
    dir2angle 'W' = 180
    dir2angle 'S' = 270
    dir2angle _ = error "wrong coordinate"

solveA :: [([Char], [Char])] -> Int
solveA instructions = abs p1 + abs p2
  where
    (p1, p2, _) = foldl navigate (0, 0, 'E') instructions'
    instructions' = map extractInst instructions
    extractInst ([inst], nStr) = (inst, read nStr)
    extractInst _ = error "wrong inst format"

main :: IO()
main = do
  text <- readFile "12.in"
  print $ solveA $ map (splitAt 1) $ lines text
