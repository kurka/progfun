
import Debug.Trace (trace)
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Vector as V

solveA :: Int -> [Int] -> Int
solveA timestamp buses = minBus * waitTime minBus
  where
    minBus = minimumBy (\a b -> waitTime a `compare` waitTime b) buses
    waitTime x = x - (timestamp `mod` x)

solveEquations :: (Int, Int) -> (Int, Int) -> Int
-- (n1*p1+a1)*p0 + d1 = (n1*p0+b1)*p1
-- (n2*p2+a2)*p0 + d1 = (n2*p0+b2)*p2
-- (n1*p1+a1) == (n2*p2+a2)
solveEquations (p1, a1) (p2, a2) = -- trace (show (p1, a1, p2, a2))
  head [n1 | n1 <- [0..], (n1*p1+a1-a2) `mod` p2 == 0]

-- find the minimum pair (a,b) that satisfies the equation (a*headbus)+delay = (b*secondBus). Note that a < secondBus and b < headbus
findEquations :: Int -> (Int, Int) -> (Int, Int)
findEquations headBus (delay, secondBus) = minimum [(i, j) | i <- [1..secondBus], j <- [1..(headBus+delay)], i*headBus+delay == j*secondBus]

-- solveB' :: [Char] -> Int
-- solveB' busStr = ((foldl1 lcm minN1s) * sndBusNum + a1)*headBus
-- solveB' :: [Char] -> [(Float, Float, Int, Int)]
-- solveB' busStr = trace (show (minN1s))--, (map fst minMultiples), (sndBusNum: tailTailBusNums)))
--   zipWith (\(ai, bi) p_i -> (((ni ai p_i) * (fromIntegral p_i) + (fromIntegral ai)) * (fromIntegral headBus),
--                              (ni ai p_i), ai, p_i)) ((a1,b1):tailMinMultiples) (sndBusNum: tailTailBusNums)
  -- map (\n -> (n*sndBusNum + a1)*headBus) minN1s
-- solveB' :: [Char] -> Int
-- solveB' busStr = ((foldl1 lcm minN1s) * sndBusNum + a1)*headBus
solveB' :: [Char] -> [(Int, Int)]
solveB' busStr = coefs
  where
    ((_, headBus):tailBuses) = map (\(a,b) -> (a, read b)) . filter ((/= "x") . snd) . zip [0..] $ splitOn "," busStr
    coefs@((a1, b1): tailMinMultiples) = map (findEquations headBus) tailBuses
    (sndBusNum: tailTailBusNums) = map snd tailBuses
    minN1s = zipWith (curry (solveEquations (sndBusNum, a1))) tailTailBusNums (map fst tailMinMultiples)
    n1 = foldl1 lcm minN1s
    ni ai p_i = fromIntegral (n1*sndBusNum+a1-ai)  / fromIntegral p_i


-- findMultiple solved (delay, newBus) = (newBus, newSolution) : solved
--   where
--     newSolution = fst head [(a, b) | b <- [0..], a <- [0..b], a* + delay == b*newBus] -- TODO: a,b limits, throw away b
--     busSums n = foldl (\n' (a, p) -> n'*p + a) n solved
findMultiple :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int, Int)]
findMultiple busesAndAs (newBus, delay) = --trace (show (newBus, delay, busesAndAs))
  (newBus, newA, (newA*prevBusesProduct + acc)) : busesAndAs
  where
    newA = head [a | a <- [0..newBus], (a*prevBusesProduct + acc + delay) `mod` newBus == 0]
    -- newA = fst $ head [(a, b) | b <- [0..], a <- [0..newBus], a*prevBusesProduct + acc + delay == b*newBus]
    -- (newA, newB) = head [(a, n) | a <- [0..newBus], a*prevBusesProduct + acc + delay == b*newBus]
    acc = foldl (\acc' (busi, ai, _) -> acc'*busi+ai) 0 busesAndAs
    prevBusesProduct = product $ map (\(x, _, _) -> x) busesAndAs


solveB'' :: [Char] -> [(Int, Int, Int)]
solveB'' busStr = coefs
  where
    busesAndDelays = map (\(a,b) -> (read b, a)) . filter ((/= "x") . snd) . zip [0..] $ splitOn "," busStr
    coefs = foldl findMultiple [] busesAndDelays


    -- coefs@((a1, b1): tailMinMultiples) = map (findEquations headBus) tailBuses
    -- (sndBusNum: tailTailBusNums) = map snd tailBuses
    -- minN1s = zipWith (curry (solveEquations (sndBusNum, a1))) tailTailBusNums (map fst tailMinMultiples)
    -- n1 = foldl1 lcm minN1s
    -- ni ai p_i = fromIntegral (n1*sndBusNum+a1-ai)  / fromIntegral p_i

addSmaller :: V.Vector Int -> V.Vector Int -> Int
addSmaller buses times = case all (== hTime) times of
  True -> hTime
  False -> --trace (show times)
           addSmaller buses (times V.// [(minBus, times V.! minBus + buses V.! minBus)])
  where
    hTime = V.head times
    minBus = V.minIndex times

solveB :: [Char] -> Int
solveB busStr = addSmaller (V.fromList buses) (V.fromList $ map (*(-1)) delays)
  where
    (delays, buses) = unzip . map (\(a,b) -> (a, read b)) . filter ((/= "x") . snd) . zip [0..] $ splitOn "," busStr
main :: IO()
main = do
  text <- readFile "13.in"
  let [sTimestamp, busIds] = lines text
  let justBuses = map read . filter (/= "x") $ splitOn "," busIds
  print $ solveA (read sTimestamp) justBuses
  print $ solveB'' busIds
