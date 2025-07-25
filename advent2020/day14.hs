-- | day 14

module Day14 where

import Data.List.Split (splitOn)
import Text.Megaparsec

data Inst = Mem Int | Mask String deriving (Show)
solveA []
main :: IO()
main = do
  text <- readFile "13.in"
  let instructions = splitOn " = " $ lines text
  print $ solveA instructions
