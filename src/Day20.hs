{-# LANGUAGE TupleSections #-}

module Day20
  ( solve1
  , solve2
  ) where

import           Data.List       (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (listToMaybe, mapMaybe)
import           Data.Sequence   (Seq (..), (><))
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Text.Parsec     hiding (Empty)

-- Type synonyms
type ID = Int
type Grid = Set (Int, Int)
type Size = Int
type Mirrored = Bool -- along vertical axis
type Image = Map (Int, Int) TransformedTile

-- Data definitions
data Tile = Tile ID Grid Size deriving Show
data Direction = N | W | S | E
data TransformedTile = TTile Tile Direction Mirrored

-- Solvers
common :: IO Image
common = do
  -- tiles <- testInput
  tiles <- input
  case placeAll tiles of
    Nothing -> error "No satisfying placements"
    Just image -> return image

solve1 :: IO ()
solve1 = do
  image <- common
  print $ cornerIdProduct image
  where
    cornerIdProduct image =
      M.foldl' (*) 1 . M.map getId $ M.filterWithKey (corner (bbox image)) image
    getId (TTile (Tile tId _ _) _ _) = tId
    corner ((ax, ay), (bx, by)) c _ =
      c `elem` [(ax, ay), (ax, by), (bx, ay), (bx, by)]

solve2 :: IO ()
solve2 = do
  image <- common
  print . minimum . map waterRoughness . transformations $ toTile image

-- Handle the input

input :: IO [Tile]
input = parseInput <$> readFile "src/inputs/input20"

testInput :: IO [Tile]
testInput = parseInput <$> readFile "src/inputs/input20_test"

parseInput :: String -> [Tile]
parseInput s = case parse tiles "" s of
  Left err  -> error $ show err
  Right res -> res
  where
    tiles = tile `sepBy1` endOfLine
    tile = parseTile <$> tileHeader <*> tileBody
    tileHeader =
      read <$> (string "Tile " *> many1 digit <* char ':' <* endOfLine)
    tileBody = many1 (oneOf "#.") `endBy1` endOfLine
    parseTile tId rows = Tile tId (toGrid rows) (length rows - 1)
    toGrid = S.fromList . concatMap tIdxs . zip [0..]
    tIdxs (r, ts) = map (\(c, _) -> (c, r)) . filter ((== '#') . snd) $
                    zip [0..] ts

-- Business logic

tileAt :: TransformedTile -> (Int, Int) -> Bool
tileAt (TTile (Tile _ grid size) rotation mirrored) coord =
  transform size rotation mirrored coord `S.member` grid
  where
    transform _ N False = id
    transform _ W False = rotateCW
    transform _ S False = rotateCW . rotateCW
    transform _ E False = rotateCCW
    transform _ N True  = mirror
    transform _ W True  = mirror . rotateCW
    transform _ S True  = mirror . rotateCW . rotateCW
    transform _ E True  = mirror . rotateCCW
    rotateCCW (x, y) = (size - y, x)
    rotateCW (x, y) = (y, size - x)
    mirror (x, y) = (size - x, y)

border :: TransformedTile -> Direction -> [Bool]
border tt@(TTile (Tile _ _ size) _ _) d = map (tileAt tt . bc d) [0..size]
  where
    bc N = (,0)
    bc S = (,size)
    bc E = (size,)
    bc W = (0,)

validate :: Image -> Maybe Image
validate image
  | all bordersMatch (M.keys image) = Just image
  | otherwise = Nothing
  where
    bordersMatch (x, y) = bottomMatches (x, y) && rightMatches (x, y)
    bottomMatches (x, y) = case (image M.!? (x, y), image M.!? (x, y+1)) of
      (Nothing, _)            -> True
      (_, Nothing)            -> True
      (Just top, Just bottom) -> border top S == border bottom N
    rightMatches (x, y) = case (image M.!? (x, y), image M.!? (x+1, y)) of
      (Nothing, _)            -> True
      (_, Nothing)            -> True
      (Just left, Just right) -> border left E == border right W

place :: Image -> (Int, Int) -> Tile -> Maybe Image
place image pos tile = listToMaybe $ mapMaybe validate possiblePlacements
  where
    possiblePlacements = place' <$> [N, W, S, E] <*> [False, True]
    place' r m = M.insert pos (TTile tile r m) image

placeAll :: [Tile] -> Maybe Image
placeAll tiles = go M.empty (Seq.singleton (0, 0)) tiles []
  where
    go img _ [] []   = Just img
    go _ Empty _ _    = Nothing
    go img (_ :<| q) [] rest = go img q rest []
    go img (p :<| q) (t:ts) rest
      | p `M.member` img = go img q (t:ts) rest
      | otherwise = case place img p t of
          Just img' -> go img' (q `appendNeighbors` p) (ts ++ rest) []
          Nothing -> go img (p :<| q) ts (t : rest)
    appendNeighbors q (x, y) =
      q >< Seq.fromList [(x, y-1), (x-1, y), (x, y+1), (x+1, y)]

bbox :: Image -> ((Int, Int), (Int, Int))
bbox = foldl' bb ((0, 0), (0, 0)) . M.keys
  where bb ((ax, ay), (bx, by)) (x, y) =
          ((min ax x, min ay y), (max bx x, max by y))

toTile :: Image -> Tile
toTile image = Tile 0 combinedTiles sizeWithoutBorders
  where
    combinedTiles = S.fromList . concatMap tilePart $ M.assocs normalized
    tilePart (i, tt) =
      map (transform i) . filter (\(x, y) -> tileAt tt (x, y)) $
      [(x, y) | x <- [1 .. tileSize - 1], y <- [1 .. tileSize - 1]]
    sizeWithoutBorders = isqrt numTiles * (tileSize - 1) - 1
    numTiles = M.size image
    tileSize = (\(TTile (Tile _ _ s) _ _) -> s) $ image M.! (0, 0)
    transform (ix, iy) (x, y) = ( ix * (tileSize - 1) + x - 1
                                , iy * (tileSize - 1) + y - 1 )
    isqrt x = floor $ sqrt (fromIntegral x :: Double)
    normalized = M.mapKeys (\(x, y) -> (x - dx, y - dy)) image
    (dx, dy) = fst $ bbox image

monsterCells :: TransformedTile -> (Int, Int) -> Maybe (Set (Int, Int))
monsterCells tt (x, y)
  | all (tileAt tt) transformedMonster = Just $ S.fromList transformedMonster
  | otherwise = Nothing
  where
    transformedMonster = map (\(dx, dy) -> (x + dx, y + dy)) monster
    monster = [ (0, 1), (1, 2), (4, 2), (5, 1), (6, 1), (7, 2), (10, 2)
              , (11, 1), (12, 1), (13, 2), (16, 2), (17, 1), (18, 0)
              , (18, 1), (19, 0)]
              --                   # 0
              -- #    ##    ##    ###1
              --  #  #  #  #  #  #   2
              -- 01234567890123456789

countMonsterCells :: TransformedTile -> Int
countMonsterCells tt@(TTile (Tile _ _ size) _ _) =
  S.size . S.unions $ mapMaybe (monsterCells tt) potentialIndices
  where
    potentialIndices = [(x, y) | x <- [0..size-19], y <- [0..size-2]]

transformations :: Tile -> [TransformedTile]
transformations t = TTile t <$> [N, W, S, E] <*> [False, True]

waterRoughness :: TransformedTile -> Int
waterRoughness tt@(TTile (Tile _ cells _) _ _) =
  S.size cells - countMonsterCells tt
