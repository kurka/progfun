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
  _ -> error "unrecognized instruction"
  where
    turn x = angle ((dir2angle face + x) `mod` 360)
    angle 0 = 'E'
    angle 90 = 'N'
    angle 180 = 'W'
    angle 270 = 'S'
    angle a = error $ "undefined angle:" ++ show a
    dir2angle 'E' = 0
    dir2angle 'N' = 90
    dir2angle 'W' = 180
    dir2angle 'S' = 270
    dir2angle _ = error "wrong coordinate"

navigateWaypoint :: ((Int, Int), (Int, Int)) -> (Char, Int) -> ((Int, Int), (Int, Int))
navigateWaypoint (sPos@(sPos1, sPos2), wPos@(wPos1, wPos2)) (inst, n) = case inst of
  'N' -> (sPos, (wPos1, wPos2+n))
  'S' -> (sPos, (wPos1, wPos2-n))
  'E' -> (sPos, (wPos1+n, wPos2))
  'W' -> (sPos, (wPos1-n, wPos2))
  'L' -> (sPos, rotate (n `mod` 360))
  'R' -> (sPos, rotate ((-n) `mod` 360))
  'F' -> ((sPos1+(n*wPos1), sPos2+(n*wPos2)), wPos)
  _ -> error "unrecognized instruction"
  where
    rotate 0 = wPos
    rotate 90 = (-wPos2, wPos1)
    rotate 180 = (-wPos1, -wPos2)
    rotate 270 = (wPos2, -wPos1)
    rotate a = error $ "undefined angle: " ++ show a ++ " " ++ show n


solveA :: [(Char, Int)] -> Int
solveA instructions = abs p1 + abs p2
  where
    (p1, p2, _) = foldl navigate (0, 0, 'E') instructions

solveB :: [(Char, Int)] -> Int
solveB instructions = abs sp1 + abs sp2
  where
    ((sp1, sp2), _) = foldl navigateWaypoint ((0,0), (10, 1)) instructions

extractInst :: ([Char], [Char]) -> (Char, Int)
extractInst ([inst], nStr) = (inst, read nStr)
extractInst _ = error "wrong inst format"

-- solveB :: [([])]
main :: IO()
main = do
  text <- readFile "12.in"
  let instructions = map (extractInst . splitAt 1) $ lines text
  print $ solveA instructions
  print $ solveB instructions
