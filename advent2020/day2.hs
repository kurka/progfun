-- | Day 2 AOC 2020

{-# LANGUAGE RecordWildCards #-}
module Day2 where

import Data.Either
import Text.Parsec

data Rule = Rule {low :: Int, high :: Int, pchar :: Char, passwd :: String}

isValidPwdCount :: Rule -> Bool
isValidPwdCount Rule {..} = low <= char_count && char_count <= high
  where
    char_count = length $ filter (==pchar) passwd

isValidPwdPos :: Rule -> Bool
isValidPwdPos Rule {..} = check low /= check high
  where
    check pos = passwd !! (pos-1) == pchar

solveA :: [Rule] -> Int
solveA = length . filter isValidPwdCount

solveB :: [Rule] -> Int
solveB = length . filter isValidPwdPos

p :: Parsec String () Rule
p = do
  low_char <- many1 digit
  _ <- char '-'
  high_char <- many1 digit
  _ <- char ' '
  c <- letter
  _ <- string ": "
  s <- many1 letter
  return $ Rule (read low_char) (read high_char) c s


main :: IO()
main = do
  text <- readFile "2.in"
  let passwdStrs = rights . map (parse p "") . lines

  print . solveA $ passwdStrs text
  print . solveB $ passwdStrs text
