module Day2 where

readInt :: String -> Int
readInt = read

commaWords :: String -> [String]
commaWords s = case dropWhile (==',') s of
  "" -> []
  s' -> w : commaWords s''
    where (w, s'') = break (==',') s'


  -- find a more efficient way to perform this transf
replace :: Int -> Int -> [Int] -> [Int]
replace value pos xs = x1 ++ value : x2
  where (x1, _:x2) = splitAt pos xs

runProg :: Int -> [Int] -> [Int]
runProg opIndex prog = case op of
  99 -> prog
  _ -> runProg (opIndex+4) newprog
  where 
    op = prog !! opIndex
    newprog = runOp op opIndex prog

-- run 4 arguments commands
runOp :: Int -> Int -> [Int] -> [Int]
runOp op opIndex prog
  | op == 1 = newProg (a+b)
  | op == 2 = newProg (a*b)
  | otherwise = error $ "Instruction not recognised: " ++ show op
  where
    a = prog !! (prog !! (opIndex+1))
    b = prog !! (prog !! (opIndex+2))
    posres = prog !! (opIndex+3)
    newProg res = replace res posres prog 


solveA :: [Int] -> Int
solveA prog = head $ runProg 0 prog'
  where
    prog' = replace 2 2 $ replace 12 1 prog


solveB :: [Int] -> [Int]
solveB prog = [100*n+v | n <- nouns, v <- verbs, testProg n v == 19690720 ]--find (== 1222684) allProgs --19690720) allProgs
  where
    nouns = [0..99]
    verbs = [0..99]
    testProg n v = head $ runProg 0 $ replace v 2 $ replace n 1 prog
    

main :: IO()
main = do
  text <- readFile "2.in"
  let ms = readInt <$> commaWords text

  print $ solveA ms
  print $ solveB ms



