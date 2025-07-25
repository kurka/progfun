-- | Day 11

-- module Day11 where

import Control.Parallel.Strategies
import qualified Data.Vector as V
-- import Debug.Trace

type Grid = V.Vector (V.Vector Char)

updateGridA :: Grid -> Grid
updateGridA grid = V.fromList [V.fromList [update (r, c) | c <- [0..(nCols-1)]] | r <- [0..(nRows-1)]]
  where
    update (r, c) = --trace ("pos: " ++ show r ++ "," ++ show c) $ case (grid !! r) !! c of
      case (grid V.! r) V.! c of
      'L' -> if adjacentOccupation (r,c) == 0 then '#' else 'L'
      '#' -> if adjacentOccupation (r,c) >= 4 then 'L' else '#'
      '.' -> '.'
      _ -> error "undefined cell"

    adjacentOccupation :: (Int, Int) -> Int
    adjacentOccupation (r, c) = sum $ map isOccupied [(r+dr,c+dc)
                                                     | dr <- [-1, 0, 1], dc <- [-1, 0, 1],
                                                      not (dr==0 && dc==0)]
    isOccupied (r, c) = if r < 0 || r >= nRows || c < 0 || c >= nCols then 0
      else case (grid V.! r) V.! c of
        'L' -> 0
        '.' -> 0
        '#' -> 1
        _ -> error "accessing wrong cell"
    nCols = length (V.head grid)
    nRows = length grid

updateGridB :: Grid -> Grid
updateGridB grid = V.fromList [V.fromList (parMap rdeepseq update [(r, c) | c <- [0..(nCols-1)]]) | r <- [0..(nRows-1)]]
  where
    update (r, c) = --trace ("pos: " ++ show r ++ "," ++ show c) $ case (grid !! r) !! c of
      case (grid V.! r) V.! c of
      'L' -> if adjacentOccupation (r,c) == 0 then '#' else 'L'
      '#' -> if adjacentOccupation (r,c) >= 5 then 'L' else '#'
      '.' -> '.'
      _ -> error "undefined cell"

    adjacentOccupation :: (Int, Int) -> Int
    adjacentOccupation (r, c) = sum $ [isOccupied (r,c) (dr, dc)
                                       | dr <- [-1, 0, 1], dc <- [-1, 0, 1],
                                       not (dr==0 && dc==0)]
    isOccupied (r, c) (dr, dc) = if (r+dr) < 0 || (r+dr) >= nRows || (c+dc) < 0 || (c+dc) >= nCols then 0
      else case (grid V.! r) V.! c of
        'L' -> isOccupied (r+dr, c+dc) (dr, dc)
        '.' -> isOccupied (r+dr, c+dc) (dr, dc)
        '#' -> 1
        _ -> error "accessing wrong cell"
    nCols = length (V.head grid)
    nRows = length grid


solve :: (Grid -> Grid) -> Grid -> Int
solve updateRule grid = case grid == grid' of
  True -> countSeats grid'
  False -> solve updateRule grid'
  where
    grid' = updateRule grid
    countSeats = V.sum . V.map (sum . V.map (\g -> if g=='#' then 1 else 0))


main :: IO()
main = do
  text <- readFile "11.in"
  let grid = V.fromList . map V.fromList $ lines text
  print $ solve updateGridA grid
  print $ solve updateGridB grid
