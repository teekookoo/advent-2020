module Day09
  ( solve1
  , solve2
  ) where

import Data.Foldable (toList)
import Data.List (inits)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as S

input :: IO [Int]
input = map read . lines <$> readFile "src/inputs/input09"

solve1 :: IO ()
solve1 = do
  stream <- input
  case invalidNumber stream of
    Nothing -> error "There should be an invalid number"
    Just n  -> print n

solve2 :: IO ()
solve2 = do
  stream <- input
  let n = fromJust $ invalidNumber stream
      xsMaybe = contiguousSubset n stream
  case xsMaybe of
    Nothing -> error $ "There should be a contiguous subset summing to " ++ show n
    Just xs -> print . uncurry (+) $ minMax xs
  where
    contiguousSubset target = listToMaybe . mapMaybe (takeCS target) . inits
    takeCS t xs = case foldr f (Just (t, [])) xs of Just (0, cs) -> Just cs
                                                    _            -> Nothing
    f _ Nothing = Nothing
    f _ (Just (0, cs)) = Just (0, cs)
    f x (Just (t, cs))
      | x > t = Nothing
      | otherwise = Just (t - x, x : cs)
    minMax (x:xs) = foldr (\x' (a, b) -> (min x' a, max x' b)) (x, x) xs
    minMax [] = error "minMax: empty list"

invalidNumber :: [Int] -> Maybe Int
invalidNumber = lookup False . validate . process
  where
    process = uncurry prefix . (\(a, b) -> (S.fromList a, b)) . splitAt 25
    prefix p@(_ :<| ns) (m:ms) = (p, m) : prefix (ns |> m) ms
    prefix _ [] = []
    prefix Empty _ = error "Prefix sequence should not be empty"
    validate = map (\(p, n) -> (sumsTo n $ toList p, n))
    sumsTo n (m:ms) = (n - m) `elem` ms || sumsTo n ms
    sumsTo _ [] = False

