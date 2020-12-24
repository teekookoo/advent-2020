module Day24
  ( solve1
  , solve2
  ) where

import           Data.List       (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Text.Parsec     hiding (count)

data Direction = E | SE | NE | W | SW | NW
data Tile = Tile Int Int
          deriving (Eq, Ord)

solve1 :: IO ()
solve1 = do
  -- tiles <- testInput
  tiles <- input
  print . S.size . blackTiles $ occurrences tiles
  where
    occurrences = foldl' (flip (M.alter f)) M.empty
    f (Just n) = Just $ n + 1
    f Nothing  = Just 1
    blackTiles = M.keysSet . M.filter (\count -> (count :: Int) `mod` 2 == 1)

solve2 :: IO ()
solve2 = putStrLn "Not implemented"

-- Input handling
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
toTile = foldl' f (Tile 0 0)
  where
    f (Tile x y) E  = Tile (x + 2) y
    f (Tile x y) W  = Tile (x - 2) y
    f (Tile x y) SE = Tile (x + 1) (y - 1)
    f (Tile x y) NW = Tile (x - 1) (y + 1)
    f (Tile x y) NE = Tile (x + 1) (y + 1)
    f (Tile x y) SW = Tile (x - 1) (y - 1)
