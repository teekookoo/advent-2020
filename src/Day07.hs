module Day07
  ( solve1
  , solve2
  ) where

import Text.Parsec
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Bag = String
type Node = ((), Bag, [Bag])
type Weight = Int
type Weights = Map Bag [(Bag, Weight)]

input :: IO ((Graph, Vertex -> Node, Bag -> Maybe Vertex), Weights)
input = do
  ruleFile <- readFile "src/inputs/input07"
  rules <- unpack $ parse bagFile "" ruleFile
  return (parseG rules, M.fromList rules)
  where
    parseG = G.graphFromEdges . map (\(b, bss) -> ((), b, map fst bss))
    unpack (Left err) = print err >> return []
    unpack (Right res) = return res
    bagFile = rule `endBy1` endOfLine <* eof
    rule = (,) <$> container <*> contents <* char '.'
    container = anyChar `manyTill` (try $ string " bags contain ")
    contents = try (countAndBag `sepBy1` (string ", ")) <|> noBags
    noBags = (\_ -> []) <$> string "no other bags"
    countAndBag = (\n c -> (c, n)) <$> num <*> color
    num = read <$> many1 digit <* space
    color = anyChar `manyTill` (try $ string " bag") <* optional (char 's')

solve1 :: IO ()
solve1 = do
  ((bagG, _, vertex), _) <- input
  let bagG' = G.transposeG bagG
  case length . G.reachable bagG' <$> (vertex "shiny gold") of
    Just n -> print $ n - 1
    Nothing -> putStrLn "Error: shiny gold not found among bags"

solve2 :: IO ()
solve2 = print "Not implemented"

lookupWeight :: Bag -> Bag -> Weights -> Maybe Weight
lookupWeight from to ws = M.lookup from ws >>= lookup to

transposeW :: Weights -> Weights
transposeW = M.fromListWith (++) . transp . M.toList
  where
    transp = concatMap (\(from, tos) -> map (\(to, w) -> (to, [(from, w)])) tos)

