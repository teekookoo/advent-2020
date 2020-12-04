module Day04
  ( solve1
  , solve2
  ) where

import Text.Parsec
import Control.Applicative (liftA2)
import Data.Bits (setBit)

data PassportField = BirthYear Int | IssueYear Int | ExpirationYear Int
                   | Height String | HairColor String | EyeColor String
                   | PassportID String | CountryID String
                   deriving Show

type PassportCandidate = [PassportField]

solve1 :: IO ()
solve1 = readFile "src/inputs/input04" >>= \s ->
  case parse passportStream "" s of
    Left err -> print err
    Right passports -> print . length $ filter isValid passports

solve2 :: IO ()
solve2 = print "Not implemented"

isValid :: PassportCandidate -> Bool
isValid = (>= (254)) . foldr checkFields (0 :: Int)
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
height = Height <$> (string "hgt:" *> liftA2 (++) (many1 digit) (many letter))
hairColor = HairColor <$> (string "hcl:" *> many1 (noneOf " \n"))
eyeColor = EyeColor <$> (string "ecl:" *> many1 (noneOf " \n"))
passportID = PassportID <$> (string "pid:" *> many1 (noneOf " \n"))
countryID = CountryID <$> (string "cid:" *> many1 (noneOf " \n"))

