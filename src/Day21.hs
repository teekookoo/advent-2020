{-# LANGUAGE TupleSections #-}

module Day21
  ( solve1
  , solve2
  ) where

import           Data.List       (intercalate, sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Text.Parsec

type Ingredient = String
type Allergen = String

solve1 :: IO ()
solve1 = do
  -- contents <- testInput
  contents <- input
  let cp = containsPossibly contents
      allIngredients = concatMap fst contents
      okIngredients = M.foldl' (S.\\) (S.fromList allIngredients) cp
      innocentOccurrences = length $ filter (`S.member` okIngredients) allIngredients
  print innocentOccurrences

solve2 :: IO ()
solve2 = do
  -- contents <- testInput
  contents <- input
  let cp = containsPossibly contents
      alg = allergens cp
  putStrLn $ toCanonical alg

input :: IO [([Ingredient], [Allergen])]
input = parseInput <$> readFile "src/inputs/input21"

testInput :: IO [([Ingredient], [Allergen])]
testInput = pure $ parseInput "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)\n"

parseInput :: String -> [([Ingredient], [Allergen])]
parseInput s = case parse ingredientFile "" s of
  Left err  -> error $ show err
  Right res -> res
  where
    ingredientFile = (ingredientLine `endBy1` endOfLine) <* eof
    ingredientLine = (,) <$> ingredientList <*> allergenList
    ingredientList = many1 lower `endBy` char ' '
    allergenList = between (string "(contains ") (char ')')
                   (many1 lower `sepBy1` string ", ")

containsPossibly :: [([Ingredient], [Allergen])]
                 -> Map Allergen (Set Ingredient)
containsPossibly rows = M.fromListWith S.intersection assocs
  where
    assocs = concatMap expand rows
    expand (is, as) = map (, S.fromList is) as

-- invert the output of containsPossibly
allergens :: Map Allergen (Set Ingredient) -> Map Ingredient Allergen
allergens = go M.empty
  where
    go alg cp
      | M.size cp == 0 = alg
      | otherwise = let (alg', cp') = iter alg cp in go alg' cp'
    iter alg cp = let (pairs, nonPairs) = M.partition ((== 1) . S.size) cp
                      iSet = S.unions . map snd $ M.assocs pairs
                  in (add alg pairs, prune iSet nonPairs)
    add = M.foldlWithKey' (\m a s -> M.insert (S.findMin s) a m)
    prune s = M.map (S.filter (`S.notMember` s))

toCanonical :: Map Ingredient Allergen -> String
toCanonical = intercalate "," . map fst . sortOn snd . M.assocs
