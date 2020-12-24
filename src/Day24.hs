{-# LANGUAGE TupleSections #-}

module Day24
  ( solve1
  , solve2
  ) where

import           Data.Function   ((&))
import           Data.List       (foldl')
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Text.Parsec     hiding (count)

data Direction = E | SE | NE | W | SW | NW
               deriving (Enum)
data Tile = Tile Int Int
          deriving (Eq, Ord)

solve1 :: IO ()
solve1 = do
  -- tiles <- testInput
  tiles <- input
  print . S.size $ blackTiles tiles

solve2 :: IO ()
solve2 = do
  -- tiles <- testInput
  tiles <- input
  let blackAfter100 = foldl' (&) (blackTiles tiles) $ replicate 100 advance
  print $ S.size blackAfter100

input :: IO [Tile]
input = parseInput <$> readFile "src/inputs/input24"

testInput :: IO [Tile]
testInput = parseInput <$> readFile "src/inputs/input24_test"

parseInput :: String -> [Tile]
parseInput s = case parse tiles "" s of
  Left err  -> error $ show err
  Right res -> res
  where
    tiles = tile `endBy1` endOfLine <* eof
    tile = toTile <$> many1 direction
    direction = choice $ map try
      [ E <$ string "e", SE <$ string "se", NE <$ string "ne"
      , W <$ string "w", SW <$ string "sw", NW <$ string "nw" ]

toTile :: [Direction] -> Tile
toTile = foldl' add (Tile 0 0)

add :: Tile -> Direction -> Tile
add (Tile x y) E  = Tile (x + 2) y
add (Tile x y) W  = Tile (x - 2) y
add (Tile x y) SE = Tile (x + 1) (y - 1)
add (Tile x y) NW = Tile (x - 1) (y + 1)
add (Tile x y) NE = Tile (x + 1) (y + 1)
add (Tile x y) SW = Tile (x - 1) (y - 1)

blackTiles :: [Tile] -> Set Tile
blackTiles = M.keysSet . M.filter isOdd . numFlips
  where
    numFlips = M.fromListWith (+) . map (,1)
    isOdd n = (n :: Int) `mod` 2 == 1

neighborSet :: Tile -> Set Tile
neighborSet = S.fromList . neighbors

neighbors :: Tile -> [Tile]
neighbors tile = map (add tile) [E .. NW]

advance :: Set Tile -> Set Tile
advance tileSet = S.filter willBeBlack tilesToConsider
  where
    tilesToConsider =
      S.foldl' (\ts t -> S.union ts (neighborSet t)) tileSet tileSet
    willBeBlack t
      | isBlack t = blackNeighborCount t `elem` [1, 2]
      | otherwise = blackNeighborCount t == 2
    isBlack t = t `S.member` tileSet
    blackNeighborCount = length . filter isBlack . neighbors
