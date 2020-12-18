module Day17
  ( solve1
  , solve2
  ) where

import           Control.Monad (guard)
import           Data.Function ((&))
import           Data.List     (foldl')
import           Data.Set      (Set)
import qualified Data.Set      as S

type Coord = (Int, Int, Int)
type Grid = Set Coord

type Coord2 = (Int, Int, Int, Int)
type Grid2 = Set Coord2

input :: IO Grid
input = toActiveSet <$> readFile "src/inputs/input17"
  where
    toActiveSet = S.fromList . map fst . filter ((== '#') . snd) .
                  concatMap coords . zip [0..] . lines
    coords (y, row) = map (\(x, e) -> ((x, y, 0), e)) $ [0..] `zip` row

input2 :: IO Grid2
input2 = toActiveSet <$> readFile "src/inputs/input17"
  where
    toActiveSet = S.fromList . map fst . filter ((== '#') . snd) .
                  concatMap coords . zip [0..] . lines
    coords (y, row) = map (\(x, e) -> ((x, y, 0, 0), e)) $ [0..] `zip` row

solve1 :: IO ()
solve1 = input >>= print . S.size . advance6
  where advance6 g = foldl' (&) g $ replicate 6 advance

solve2 :: IO ()
solve2 = input2 >>= print . S.size . advance6
  where advance6 g = foldl' (&) g $ replicate 6 advance2

advance :: Grid -> Grid
advance g = S.fromList $ filter activeNext allCoords
  where
    allCoords = [(x, y, z) | x <- [xMin-1 .. xMax+1], y <- [yMin-1 .. yMax+1]
                           , z <- [zMin-1 .. zMax+1]]
    ((xMin, yMin, zMin), (xMax, yMax, zMax)) = (foldWith min, foldWith max)
    foldWith op = S.foldr'
      (\(x, y, z) (x', y', z') -> (op x x', op y y', op z z')) (0, 0, 0) g
    activeNext c = determine (c `S.member` g) (numActiveNeighbors c)
    determine True n | n == 2 || n == 3 = True
                     | otherwise = False
    determine False 3 = True
    determine False _ = False
    numActiveNeighbors c@(x, y, z) = length $ do
      x' <- [x-1 .. x+1]
      y' <- [y-1 .. y+1]
      z' <- [z-1 .. z+1]
      let c' = (x', y', z')
      guard (c /= c' && c' `S.member` g)
      return c'

advance2 :: Grid2 -> Grid2
advance2 g = S.fromList $ filter activeNext allCoords
  where
    allCoords = [(x, y, z, w) | x <- [xMin-1 .. xMax+1], y <- [yMin-1 .. yMax+1]
                              , z <- [zMin-1 .. zMax+1], w <- [wMin-1 .. wMax+1]]
    ((xMin, yMin, zMin, wMin), (xMax, yMax, zMax, wMax)) =
      (foldWith min, foldWith max)
    foldWith op = S.foldr'
      (\(x, y, z, w) (x', y', z', w') -> (op x x', op y y', op z z', op w w'))
      (0, 0, 0, 0) g
    activeNext c = determine (c `S.member` g) (numActiveNeighbors c)
    determine True n | n == 2 || n == 3 = True
                     | otherwise = False
    determine False 3 = True
    determine False _ = False
    numActiveNeighbors c@(x, y, z, w) = length $ do
      x' <- [x-1 .. x+1]
      y' <- [y-1 .. y+1]
      z' <- [z-1 .. z+1]
      w' <- [w-1 .. w+1]
      let c' = (x', y', z', w')
      guard (c /= c' && c' `S.member` g)
      return c'
