{-# LANGUAGE OverloadedStrings #-}
-- | Day 8

module Day8 where


import Data.Either
import qualified Data.Vector as V
import Data.Void
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser = Parsec Void String

data Inst = Acc Int | Jmp Int | Nop Int deriving (Show)

parseInst :: Parser Inst
parseInst = choice
  [Acc <$ "acc " <*> signed space decimal,
   Jmp <$ "jmp " <*> signed space decimal,
   Nop <$ "nop " <*> signed space decimal -- TODO: remove the int from data type
  ]

runProgram :: V.Vector Inst -> S.Set Int-> Int -> Int -> (Int, Int)
runProgram ops visited pos acc
  | pos == length ops = (acc, pos)
  | pos `S.member` visited = (acc, pos)
  | otherwise = case ops V.! pos of
      Acc x -> runProgram ops (S.insert pos visited) (pos+1) (acc+x)
      Jmp x -> runProgram ops (S.insert pos visited) (pos+x) acc
      Nop _ -> runProgram ops (S.insert pos visited) (pos+1) acc


solveA :: V.Vector Inst -> Int
solveA ops = fst $ runProgram ops S.empty 0 0

opposite :: Inst -> Inst
opposite (Jmp x) = Nop x
opposite (Nop x) = Jmp x
opposite (Acc x) = Acc x

replaceAndRun :: V.Vector Inst -> Int -> (Int, Int)
replaceAndRun ops posReplacement = runProgram ops' S.empty 0 0
  where ops' = ops V.// [(posReplacement, opposite (ops V.! posReplacement))]

solveB :: V.Vector Inst -> Int
solveB ops = fst . head . filter ((== nOps) . snd) $ map (replaceAndRun ops) [0..(nOps-1)]
  where nOps = length ops

main :: IO()
main = do
  text <- readFile "8.in"
  let ops = V.fromList . rights . map (parse parseInst "") $ lines text
  print $ solveA ops
  print $ solveB ops
