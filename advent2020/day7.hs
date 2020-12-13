{-# LANGUAGE OverloadedStrings #-}
-- | Day 7

module Day7 where

import qualified Data.Map as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


type Parser = Parsec Void String
data Bag = Bag String [(Int, String)] deriving (Eq, Show, Ord)

colorParser :: Parser String
colorParser = some letterChar <> string " " <> some letterChar <* " bag" <* optional (char 's')

contentParser :: Parser (Int, String)
contentParser = (,) <$> decimal <* spaceChar <*> colorParser

bagParser :: Parser (String, Bag)
bagParser = do
  color <- colorParser
  _ <- " contain "
  contents <- ([] <$ string "no other bags") <|> (contentParser `sepBy` ", ")
  _ <- "."
  return (color, Bag color contents)

bagsParser :: Parser (M.Map String Bag)
bagsParser = M.fromList <$> sepEndBy bagParser "\n"

findColor :: M.Map String Bag -> String -> Bag -> Bool
findColor _ color (Bag color' _) | color == color' = True
findColor _ _ (Bag _ []) = False
findColor bags color (Bag _ contents) = any (findColor bags color . (M.!) bags . snd) contents

countBagsInside :: M.Map String Bag -> Bag -> Int
countBagsInside bags (Bag _ contents) = sum $ map countContent contents
  where
    countContent (bCount, bagName) = bCount + bCount*countBagsInside bags (bags M.! bagName)

-- O(n^2) search (not very efficient)
solveA :: M.Map String Bag -> Int
solveA bags = flip (-) 1 . length $ M.filter (findColor bags "shiny gold") bags

solveB :: M.Map String Bag -> Int
solveB bags = countBagsInside bags (bags M.! "shiny gold")

main :: IO()
main = do
  text <- readFile "7.in"
  let Right bagTree = parse bagsParser "" text
  print $ solveA bagTree
  print $ solveB bagTree
