module Day6 where

import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S


parseInput :: String -> (M.Map String String, S.Set String)
parseInput = foldl parsePair (M.empty, S.empty) . map (splitOn ")") . lines
  where
    parsePair (curDag, curChildren)  [father, child] = (M.insert child father curDag,
                                                        S.insert child curChildren) --(S.delete father curChildren))
    parsePair (_, _) _ = error "There is something wrong with the input"
    

-- very inneficient method, visit common paths multiple times.. but fast enough for this problem
solveA :: M.Map String String -> [String] -> Int
solveA dag (child: children) = countParents child  + solveA dag children
  where
    countParents c = case M.lookup c dag of
      Nothing -> 0 -- c is root node
      Just p -> 1 + countParents p  -- count parents of parent
solveA _ [] = 0

solveB :: M.Map String String -> Int
solveB dag = lenTrip sanParents + lenTrip youParents
  where
    listParents acc c = case M.lookup c dag of
      Nothing -> acc -- found root!
      Just p -> listParents (p:acc) p -- continue list
    sanParents = listParents [] "SAN"
    youParents = listParents [] "YOU"
    lenCommonPrefix (l1:ls1) (l2:ls2) acc
      | l1 == l2 = lenCommonPrefix ls1 ls2 acc+1
      | otherwise = acc
    lenCommonPrefix _ _ acc = acc
    splitPoint = lenCommonPrefix sanParents youParents 0
    lenTrip = length . snd . splitAt splitPoint
    

  
main :: IO()
main = do
  text <- readFile "6.in"
  let (dag, children) = parseInput text
  print $ solveA dag (S.toList children)
  print $ solveB dag 
