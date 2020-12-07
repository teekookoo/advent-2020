module Day07
  ( solve1
  , solve2
  ) where

import Text.Parsec
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

type Bag = String
type Bagspec = (Int, Bag)

input :: IO [(Bag, [Bagspec])]
input = readFile "src/inputs/input07" >>= unpack . parse bagFile ""
  where
    unpack (Left err) = print err >> return []
    unpack (Right res) = return res
    bagFile = rule `endBy1` endOfLine <* eof
    rule = (,) <$> container <*> contents <* char '.'
    container = anyChar `manyTill` (try $ string " bags contain ")
    contents = try (countAndBag `sepBy1` (string ", ")) <|> noBags
    noBags = (\_ -> []) <$> string "no other bags"
    countAndBag = (,) <$> num <*> color
    num = read <$> many1 digit <* space
    color = anyChar `manyTill` (try $ string " bag") <* optional (char 's')

solve1 :: IO ()
solve1 = input >>= \rules ->
  print . S.size $ possibleContainers (containerMap rules) "shiny gold"

solve2 :: IO ()
solve2 = print "Not implemented"

containerMap :: [(Bag, [Bagspec])] -> Map Bag [Bag]
containerMap = M.fromListWith (++) . invert
  where invert = concatMap (\(b, bs) -> map (\(_, bb) -> (bb, [b])) bs)

possibleContainers :: Map Bag [Bag] -> Bag -> Set Bag
possibleContainers m b = foldl' S.union (S.fromList containers) parentContainers
  where
    containers = M.findWithDefault [] b m
    parentContainers = map (possibleContainers m) containers

