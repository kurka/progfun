module Day6 where

import qualified Data.Vector as V
import Data.Vector ((//), (!))
import Data.List.Split
import Data.List (permutations)
-- import Debug.Trace

readInt :: String -> Int
readInt = read

commaWords :: String -> [String]
commaWords s = case dropWhile (==',') s of
  "" -> []
  s' -> w : commaWords s''
    where (w, s'') = break (==',') s'


 -- TODO: find a more efficient way to perform this transf
replace :: Int -> Int -> V.Vector Int -> V.Vector Int
replace pos value xs = xs // [(pos, value)]


getDigit :: Int -> Int -> Int
getDigit n x = (x `div` 10^(n-1)) `mod` 10 

runProg :: Int -> V.Vector Int -> [Int] -> [Int] -> [Int]
runProg pos prog inputs outputs =
  -- case trace ("tr: " ++ show [pos, opInst, a, b, c] ++ show prog) opCode of
  case opCode of
    99 -> outputs
    1 -> runProg (pos+4) (replace c (prog!a+prog!b) prog) inputs outputs
    2 -> runProg (pos+4) (replace c (prog!a*prog!b) prog) inputs outputs
    3 -> runProg (pos+2) (replace a (head inputs) prog) (tail inputs) outputs
    4 -> runProg (pos+2) prog inputs (prog!a:outputs)
    5 -> runProg (if prog!a /= 0 then prog!b else pos+3) prog inputs outputs -- jump-if-true
    6 -> runProg (if prog!a == 0 then prog!b else pos+3) prog inputs outputs -- jump-if-false
    7 -> runProg (pos+4) (replace c (if prog!a < prog!b then 1 else 0) prog) inputs outputs -- less than
    8 -> runProg (pos+4) (replace c (if prog!a == prog!b then 1 else 0) prog) inputs outputs -- less than
    _ -> error $ "Instruction not recognised: " ++ show opInst
  where
    opInst = prog ! pos
    (opCode, modeA, modeB, modeC) = (opInst `mod` 100, getDigit 3 opInst, getDigit 4 opInst, getDigit 5 opInst)
    getParameter idx mode | mode == 0 = prog ! idx -- position mode 
                          | otherwise = idx -- immediate mode
    a = getParameter (pos+1) modeA
    b = getParameter (pos+2) modeB
    c = getParameter (pos+3) modeC


solveA :: V.Vector Int -> Int
solveA prog = maximum $ map testPhases $ permutations [0..4]
  where
    testPhases = foldl (\acc phase ->  head $ runProg 0 prog [phase, acc] []) 0


    

main :: IO()
main = do
  text <- readFile "7.in"
  -- let ms = readInt <$> commaWords text
  let ms = V.fromList $ read <$> splitOn "," text  

  print $ solveA ms
  -- print $ solveB ms


