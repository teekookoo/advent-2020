module Day03
  ( solve1
  , solve2
  ) where

import Data.Set (Set)
import qualified Data.Set as S

data TreeMap = TreeMap Int Int (Set (Int, Int))
type Slope = (Int, Int)

solve1 :: IO ()
solve1 = do
  tm <- input
  print $ treesOnPath tm (3, 1)

solve2 :: IO ()
solve2 = do
  tm <- input
  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  print . product $ map (treesOnPath tm) slopes

input :: IO TreeMap
input = do
  inputList <- lines <$> readFile "src/inputs/input03"
  let height = length inputList
  let width = length (head inputList)
  let indexList = [(y, x) | y <- [0..height-1], x <- [0..width-1]]
  let indexedInput = indexList `zip` (concat inputList)
  let treeCoordinates = S.fromList . map fst . filter ((== '#') . snd)
  return (TreeMap width height (treeCoordinates indexedInput))

treesOnPath :: TreeMap -> Slope -> Int
treesOnPath (TreeMap width height trees) (dx, dy) =
  S.size $ S.intersection trees (S.fromList path)
  where
    path = [(i * dy, i * dx `mod` width)| i <- [0 .. height - 1 `div` dy]]
