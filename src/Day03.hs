module Day03
  ( solve1
  , solve2
  ) where

import Data.Set (Set)
import qualified Data.Set as S

data TreeMap = TreeMap Int Int (Set (Int, Int))
type Slope = Int

solve1 :: IO ()
solve1 = do
  tm <- input
  putStrLn . show $ treesOnPath tm 3

solve2 :: IO ()
solve2 = putStrLn "Not implemented"

input :: IO TreeMap
input = do
  inputList <- lines <$> readFile "src/inputs/input03"
  let height = length inputList
  let width = length (head inputList)
  let indexList = [(y, x) | y <- [0..height-1], x <- [0..width-1]]
  let indexedInput = indexList `zip` (concat inputList)
  let treeCoordinates = S.fromList . map fst . filter ((== '#') . snd) $ indexedInput
  return (TreeMap width height treeCoordinates)

treesOnPath :: TreeMap -> Slope -> Int
treesOnPath (TreeMap width height trees) dx = S.size $ S.intersection trees path
  where
    path = S.fromList [(y, y * dx `mod` width) | y <- [0..height-1]]
