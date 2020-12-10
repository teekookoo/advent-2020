module Day10
  ( solve1
  , solve2
  ) where

import Data.Array.Unboxed
import qualified Data.IntMap.Strict as M
import Data.List (sort)

input :: IO [Int]
input = process <$> readFile "src/inputs/input10"
  where process = (\xs -> 0 : xs ++ [last xs + 3]) . sort . map read . lines

solve1 :: IO ()
solve1 = do
  numbers <- input
  let (ones, threes) = countOnesAndThrees . diffs $ numbers
  print (ones * threes :: Int)
  where
    diffs xs = zipWith (-) (tail xs) xs
    countOnesAndThrees = foldr f (0, 0)
    f 1 (ones, threes) = (ones + 1, threes)
    f 3 (ones, threes) = (ones, threes + 1)
    f _ p = p

solve2 :: IO ()
solve2 = do
  numbers <- input
  let
    ns = listArray (1, length numbers) numbers :: Array Int Int
    (iMin, iMax) = bounds ns
    canSkip i n
      | i + n >= iMax = False
      | otherwise     = ns ! (i + n + 1) - ns ! i <= 3
    f i ps =
      let numPaths = map (\n -> ps M.! (i + n + 1))
          p = sum . numPaths . filter (canSkip i) $ [0, 1, 2]
      in  M.insert i p ps
    paths = foldr f (M.fromList [(iMax, 1)]) . init $ [iMin .. iMax]
  print (paths M.! 1 :: Int)
