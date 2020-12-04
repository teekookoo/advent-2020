module Day04
  ( solve1
  , solve2
  ) where

import Text.Parsec
import Control.Applicative (liftA2)
import Control.Monad (filterM)
import Data.Bits (setBit)

data PassportField = BirthYear Int | IssueYear Int | ExpirationYear Int
                   | Height String | HairColor String | EyeColor String
                   | PassportID String | CountryID String
                   deriving Show

type PassportCandidate = [PassportField]


passports :: IO [PassportCandidate]
passports = readFile "src/inputs/input04" >>= \s ->
  case parse passportStream "" s of
    Left err -> print err >> return []
    Right passports' -> return passports'

solve1 :: IO ()
solve1 = passports >>= print . length . filter hasCorrectFields

solve2 :: IO ()
solve2 = passports >>= print . length . fValid
  where fValid = filter hasValidFields . filter hasCorrectFields


hasCorrectFields :: PassportCandidate -> Bool
hasCorrectFields = (>= (254)) . foldr checkFields (0 :: Int)
  where
    sb = flip setBit
    checkFields (CountryID _) = sb 0
    checkFields (PassportID _) = sb 1
    checkFields (EyeColor _) = sb 2
    checkFields (HairColor _) = sb 3
    checkFields (Height _) = sb 4
    checkFields (ExpirationYear _) = sb 5
    checkFields (IssueYear _) = sb 6
    checkFields (BirthYear _) = sb 7

hasValidFields :: PassportCandidate -> Bool
hasValidFields = all validContents

validContents :: PassportField -> Bool
validContents (BirthYear y) = y >= 1920 && y <= 2002
validContents (IssueYear y) = y >= 2010 && y <= 2020
validContents (ExpirationYear y) = y >= 2020 && y <= 2030
validContents (Height hs) = case parse (heightSpec <* eof) "" hs of
  Left _ -> False
  Right (h, unit) -> (unit == "cm" && h >= 150 && h <= 193) ||
                     (unit == "in" && h >= 59 && h <= 76)
  where
    heightSpec :: Parsec String () (Int, String)
    heightSpec = pack (many1 digit) (try (string "cm") <|> string "in")
    pack = liftA2 (\a b -> (read a, b))
validContents (HairColor hc) = case parse (hairColorSpec <* eof) "" hc of
  Left _ -> False
  Right _ -> True
  where
    hairColorSpec = string "#" <> sixHexDigits
    sixHexDigits = foldr (liftA2 (:)) (pure "") (take 6 $ repeat hexDigit)
validContents (EyeColor ec) = case parse (eyeColorSpec <* eof) "" ec of
  Left _ -> False
  Right _ -> True
  where
    eyeColorSpec = choice $ map try [ string "amb", string "blu", string "brn"
                                    , string "gry", string "grn", string "hzl"
                                    , string "oth"]
validContents (PassportID pid) = case parse (pidSpec <* eof) "" pid of
  Left _ -> False
  Right _ -> True
  where
    pidSpec = foldr (liftA2 (:)) (pure []) (take 9 $ repeat digit)
validContents (CountryID _) = True

-- parser stuff
passportStream = sepBy passportBlock endOfLine <* eof
passportBlock = endBy passportField space
passportField :: Parsec String () PassportField
passportField = choice $ map try [ birthYear, issueYear, expirationYear
                                 , height, hairColor, eyeColor
                                 , passportID, countryID ]
birthYear = BirthYear . read <$> (string "byr:" *> many1 digit)
issueYear = IssueYear . read <$> (string "iyr:" *> many1 digit)
expirationYear = ExpirationYear . read <$> (string "eyr:" *> many1 digit)
height = Height <$> (string "hgt:" *> (many1 digit) <> (many letter))
hairColor = HairColor <$> (string "hcl:" *> many1 (noneOf " \n"))
eyeColor = EyeColor <$> (string "ecl:" *> many1 (noneOf " \n"))
passportID = PassportID <$> (string "pid:" *> many1 (noneOf " \n"))
countryID = CountryID <$> (string "cid:" *> many1 (noneOf " \n"))

