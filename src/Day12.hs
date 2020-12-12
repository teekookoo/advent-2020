module Day12
  ( solve1
  , solve2
  ) where

import           Data.Char   (isDigit)
import           Data.List   (foldl')
import           Text.Parsec

data Action = MoveNorth   Int
            | MoveSouth   Int
            | MoveEast    Int
            | MoveWest    Int
            | TurnLeft    Int
            | TurnRight   Int
            | MoveForward Int

type Position = (Int, Int)
type Direction = Int
data FerryState = FS Position Direction
                | FS2 Position Position

instance Show Action where
  show action = case action of
    MoveNorth n   -> f 'N' n
    MoveSouth n   -> f 'S' n
    MoveEast n    -> f 'E' n
    MoveWest n    -> f 'W' n
    TurnLeft n    -> f 'L' n
    TurnRight n   -> f 'R' n
    MoveForward n -> f 'F' n
    where f a b = a : show b

instance Read Action where
  readsPrec _ (a:rest)
    | a == 'N' = f MoveNorth rest
    | a == 'S' = f MoveSouth rest
    | a == 'E' = f MoveEast rest
    | a == 'W' = f MoveWest rest
    | a == 'L' = f TurnLeft rest
    | a == 'R' = f TurnRight rest
    | a == 'F' = f MoveForward rest
    | otherwise = []
    where f b r = case span isDigit r of (n, s) -> [(b (read n), s)]
  readsPrec _ _ = []

testInput :: IO [Action]
testInput = pure . map read $ ["F10", "N3", "F7", "R90", "F11"]

input :: IO [Action]
input = do
  contents <- readFile "src/inputs/input12"
  case parse navigationInstructions "" contents of
    Left err  -> error (show err)
    Right res -> pure res
  where
    navigationInstructions = action `endBy` char '\n' <* eof
    action = (\a b -> read (a:b)) <$> oneOf "NSEWLRF" <*> many1 digit

solve1 :: IO ()
solve1 = do
  -- actions <- testInput
  actions <- input
  let FS (x, y) _ = foldl' (\fs a -> a fs) startState (map advance actions)
      startState = FS (0, 0) 0
  print (abs x + abs y)

solve2 :: IO ()
solve2 = do
  -- actions <- testInput
  actions <- input
  let FS2 (x, y) _ = foldl' (\fs a -> a fs) startState (map advance actions)
      startState = FS2 (0, 0) (10, 1)
  print (abs x + abs y)

advance :: Action -> FerryState -> FerryState
advance (MoveNorth n) (FS (x, y) d)   = FS (x, y+n) d
advance (MoveSouth n) (FS (x, y) d)   = FS (x, y-n) d
advance (MoveEast n) (FS (x, y) d)    = FS (x+n, y) d
advance (MoveWest n) (FS (x, y) d)    = FS (x-n, y) d
advance (TurnLeft n) (FS p d)         = FS p ((d + n) `mod` 360)
advance (TurnRight n) (FS p d)        = FS p ((d - n) `mod` 360)
advance (MoveForward n) fs@(FS _ 0)   = advance (MoveEast n) fs
advance (MoveForward n) fs@(FS _ 90)  = advance (MoveNorth n) fs
advance (MoveForward n) fs@(FS _ 180) = advance (MoveWest n) fs
advance (MoveForward n) fs@(FS _ 270) = advance (MoveSouth n) fs
advance _ (FS _ _) = error "Direction should be one of 0, 90, 180, 270"

advance (MoveNorth n) (FS2 p (x, y)) = FS2 p (x, y+n)
advance (MoveSouth n) (FS2 p (x, y)) = FS2 p (x, y-n)
advance (MoveEast n) (FS2 p (x, y)) = FS2 p (x+n, y)
advance (MoveWest n) (FS2 p (x, y)) = FS2 p (x-n, y)
advance (TurnLeft n) (FS2 p wp) = FS2 p (rotate n wp)
  where
    rotate 0 (x, y)   = (x, y)
    rotate 90 (x, y)  = (-y, x)
    rotate 180 (x, y) = (-x, -y)
    rotate 270 (x, y) = (y, -x)
    rotate _ _        = undefined
advance (TurnRight n) fs = advance (TurnLeft ((-n) `mod` 360)) fs
advance (MoveForward n) (FS2 (x, y) (dx, dy)) =
  FS2 (x + n*dx, y + n*dy) (dx, dy)
