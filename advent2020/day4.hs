{-# LANGUAGE OverloadedStrings #-}
-- |  Day 4

module Day4 where

import Control.Monad
import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Value a = Either String a

data Field
  = Byr (Value Int)
  | Iyr (Value Int)
  | Eyr (Value Int)
  | Hgt (Value (Int, String))
  | Hcl (Value String)
  | Ecl (Value String)
  | Pid (Value String)
  | Cid (Value String)
  deriving (Eq, Show)

type Passport = [Field]

type Parser = Parsec Void String

(<=>) :: Ord a => a -> a -> a -> Bool
x <=> y = \z -> x <= z && z <= y

parseAny :: Parser String
parseAny = some (alphaNumChar <|> oneOf ['#', ':'])

parseNumber :: Parser Int
parseNumber = decimal <* notFollowedBy (satisfy isAlpha)

endHere :: Parser ()
endHere = notFollowedBy (satisfy isAlphaNum)

validateHgt :: (Int, String) -> Bool
validateHgt (height, unit) = case unit of
  "cm" -> (150 <=> 193) height
  "in" -> (59 <=> 76) height
  _ -> error "Parser should have prevented this case"

parseAndValidate :: Parser a -> (a -> Bool) -> Parser (Value a)
parseAndValidate parser validator = try right <|> left
  where
    -- this part was copied from somewhere else. I'm still not very familiar with it
    right = parser >>= \res -> guard (validator res) >> return (Right res)
    left = Left <$> parseAny

parseEither :: Parser a -> Parser (Value a)
parseEither rightParser = try (Right <$> rightParser) <|> Left <$> parseAny



fieldParser :: Parser Field
fieldParser = choice
  [ Byr <$> ("byr:" *> parseEither parseNumber),-- (1920 <=> 2002)),
    Iyr <$> ("iyr:" *> parseEither parseNumber),-- (2010 <=> 2020)),
    Eyr <$> ("eyr:" *> parseEither parseNumber),-- (2020 <=> 2030)),
    Hgt <$> ("hgt:" *> parseEither ((,) <$> decimal <*> ("in" <|> "cm"))),-- validateHgt),
    Hcl <$> ("hcl:" *> parseEither ("#" *> count 6 (satisfy isHexDigit) <* endHere)),-- ((6==) . length)),
    Ecl <$> ("ecl:" *> parseEither (count 3 (satisfy isAlpha) <* endHere)),-- (\e -> elem e $ words "amb blu brn gry grn hzl oth")),
    Pid <$> ("pid:" *> parseEither (count 9 (satisfy isDigit) <* endHere)),-- ((9==) . length)),
    Cid <$> ("cid:" *> parseEither parseAny)-- (const True))
  ]


passportParser :: Parser Passport
passportParser = sepEndBy fieldParser (" " <|> "\n")

linesParser :: Parser [Passport]
linesParser = sepBy passportParser "\n"

solveA :: Either a [Passport] -> Int
solveA (Right pwds) = length $ filter isValid pwds
  where
    -- TODO: also check if the 7 are unique
    isValid = (7 ==) . length . filter notCid
    notCid (Cid _) = False
    notCid _ = True
solveA (Left _) = error "Parse error!"


solveB :: Either a [Passport] -> Int
solveB (Right pwds) = length $ filter isValid pwds
  where
    isValid = (7 ==) . length . filter isValidField
    isValidField (Byr (Right y)) = (1920 <=> 2002) y
    isValidField (Iyr (Right y)) = (2010 <=> 2020) y
    isValidField (Eyr (Right y)) = (2020 <=> 2030) y
    isValidField (Hgt (Right (h, u))) = case u of
      "cm" -> (150 <=> 193) h
      "in" -> (59 <=> 76) h
    isValidField (Hcl (Right _)) = True
    isValidField (Ecl (Right e)) = elem e ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    isValidField (Pid (Right _)) = True
    isValidField (Cid (Right _)) = True
    isValidField _ = False
solveB (Left _) = error "Parse error!"


main :: IO()
main = do
  text <- readFile "4.in"
  let passports = parse linesParser "" text
  print . solveA $ passports
  print . solveB $ passports
  print passports
 
