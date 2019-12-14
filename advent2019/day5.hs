
module Day5 where

import Data.List.Split
import Debug.Trace

readInt :: String -> Int
readInt = read

commaWords :: String -> [String]
commaWords s = case dropWhile (==',') s of
  "" -> []
  s' -> w : commaWords s''
    where (w, s'') = break (==',') s'


 -- TODO: find a more efficient way to perform this transf
replace :: Int -> Int -> [Int] -> [Int]
replace pos value xs = x1 ++ value : x2
  where
    (x1, xx2) = splitAt pos xs
    x2 | null xx2 = []
       | not (null xx2) = tail xx2


getDigit :: Int -> Int -> Int
getDigit n x = (x `div` 10^(n-1)) `mod` 10 

runProg :: Int -> [Int] -> [Int] -> [Int]
runProg pos prog outputs =
  -- case trace ("tr: " ++ show [pos, opInst, a, b, c] ++ show prog) opCode of
  case opCode of
    99 -> outputs
    1 -> runProg (pos+4) (replace c (prog!!a+prog!!b) prog) outputs
    2 -> runProg (pos+4) (replace c (prog!!a*prog!!b) prog) outputs
    -- 3 -> runProg (pos+2) (replace a 5 prog) outputs -- FIXME: cheating, as the only input of the exercise is 1. Will need to implement IO if want arbitrary inputs
    4 -> runProg (pos+2) prog (prog!!a:outputs)
    5 -> runProg (if prog!!a /= 0 then prog!!b else pos+3) prog outputs -- jump-if-true
    6 -> runProg (if prog!!a == 0 then prog!!b else pos+3) prog outputs -- jump-if-false
    7 -> runProg (pos+4) (replace c (if prog!!a < prog!!b then 1 else 0) prog) outputs -- less than
    8 -> runProg (pos+4) (replace c (if prog!!a == prog!!b then 1 else 0) prog) outputs -- less than
    _ -> error $ "Instruction not recognised: " ++ show opInst
  where
    opInst = prog !! pos
    (opCode, modeA, modeB, modeC) = (opInst `mod` 100, getDigit 3 opInst, getDigit 4 opInst, getDigit 5 opInst)
    getParameter idx mode | mode == 0 = prog !! idx -- position mode 
                          | otherwise = idx -- immediate mode
    a = getParameter (pos+1) modeA
    b = getParameter (pos+2) modeB
    c = getParameter (pos+3) modeC
    

solveA :: [Int] -> [Int]
solveA prog = runProg 2 prog' []
  -- fixme: not implementing op 3 as I'm not comfortable with side effects yet
  where prog' = replace (prog!!1) 1 prog


solveB :: [Int] -> [Int]
solveB prog = runProg 2 prog' []
  -- fixme: not implementing op 3 as I'm not comfortable with side effects yet
  where prog' = replace (prog!!1) 5 prog
-- solveB :: [Int] -> [Int]
-- solveB prog = [100*n+v | n <- nouns, v <- verbs, testProg n v == 19690720 ]--find (== 1222684) allProgs --19690720) allProgs
--   where
--     nouns = [0..99]
--     verbs = [0..99]
--     testProg n v = head $ runProg 0 $ replace v 2 $ replace n 1 prog
    

main :: IO()
main = do
  text <- readFile "5.in"
  -- let ms = readInt <$> commaWords text
  let ms = read <$> splitOn "," text  

  print $ solveA ms
  print $ solveB ms



