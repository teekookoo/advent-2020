module Day11
  ( solve1
  , solve2
  ) where

import Data.Array.Unboxed
import Data.List (find)
import Data.Maybe (mapMaybe)

data Seat
  = Occupied
  | Empty
  | Floor
  deriving (Show, Eq)

data Direction
  = N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | NW
  deriving (Enum)

type Seats = Array (Int, Int) Seat

parseInput :: String -> Seats
parseInput = toArray . parseSeats
  where
    toArray xs = listArray (arrayDims xs) (concat xs)
    arrayDims xs = ((1, 1), (length xs, length (head xs)))
    parseSeats = map (map parseSeat) . lines
    parseSeat '#' = Occupied
    parseSeat 'L' = Empty
    parseSeat _ = Floor

-- testInput :: IO Seats
-- testInput =
--   pure . parseInput $
--   "#.##.##.##\n#######.##\n#.#.#..#..\n####.##.##\n#.##.##.##\n#.#####.##\n..#.#.....\n##########\n#.######.#\n#.#####.##"

input :: IO Seats
input = parseInput <$> readFile "src/inputs/input11"

solve1 :: IO ()
solve1 = do
  -- initialSeats <- testInput
  initialSeats <- input
  case numOccupiedAtEnd initialSeats of
    Nothing -> error "This should not happen"
    Just n -> print n
  -- mapM_ (putStrLn . pprint) . take 5 . iterate advance $ initialSeats
  where
    numOccupiedAtEnd s = count Occupied <$> (steadyState . iterate advance $ s)
    steadyState xs = fst <$> find (uncurry (==)) (xs `zip` tail xs)

solve2 :: IO ()
solve2 = do
  -- initialSeats <- testInput
  initialSeats <- input
  case numOccupiedAtEnd initialSeats of
    Nothing -> error "This should not happen"
    Just n -> print n
  -- mapM_ (putStrLn . pprint) . take 6 . iterate advance2 $ initialSeats
  where
    numOccupiedAtEnd s = count Occupied <$> (steadyState . iterate advance2 $ s)
    steadyState xs = fst <$> find (uncurry (==)) (xs `zip` tail xs)

advance :: Seats -> Seats
advance seats = seats // changes seats
  where
    changes = map update . range . bounds
    update i = (i, nextState (seats ! i) (count Occupied . adjacents $ i))
    adjacents (i, j) =
      map (seats !) . filter (inRange (bounds seats)) . filter ((i, j) /=) $
      [(ii, jj) | ii <- [i - 1 .. i + 1], jj <- [j - 1 .. j + 1]]
    nextState Occupied numOccupiedAdjacent
      | numOccupiedAdjacent >= 4 = Empty
      | otherwise = Occupied
    nextState Empty numOccupiedAdjacent
      | numOccupiedAdjacent == 0 = Occupied
      | otherwise = Empty
    nextState Floor _ = Floor

advance2 :: Seats -> Seats
advance2 seats = seats // changes seats
  where
    changes = map update . range . bounds
    update i = (i, nextState (seats ! i) (count Occupied . visible $ i))
    visible i = mapMaybe (visibleDir i) [N .. NW]
    visibleDir i d =
      find (/= Floor) .
      map (seats !) . takeWhile (inRange (bounds seats)) . tail $
      iterate (next d) i
    next N (i, j) = (i - 1, j)
    next S (i, j) = (i + 1, j)
    next W (i, j) = (i, j - 1)
    next E (i, j) = (i, j + 1)
    next NE i = next N . next E $ i
    next NW i = next N . next W $ i
    next SE i = next S . next E $ i
    next SW i = next S . next W $ i
    nextState Occupied numOccupiedVisible
      | numOccupiedVisible >= 5 = Empty
      | otherwise = Occupied
    nextState Empty numOccupiedVisible
      | numOccupiedVisible == 0 = Occupied
      | otherwise = Empty
    nextState Floor _ = Floor

count :: Foldable t => Seat -> t Seat -> Int
count seatType = foldr f 0
  where
    f s c
      | s == seatType = c + 1
      | otherwise = c
-- pprint :: Seats -> String
-- pprint ss =
--   addBreaks (snd . snd $ bounds ss) . foldr (\s str -> toSym s : str) "" $ ss
--   where
--     addBreaks _ "" = ""
--     addBreaks n s  = take n s ++ "\n" ++ addBreaks n (drop n s)
--     toSym Occupied = '#'
--     toSym Empty    = 'L'
--     toSym Floor    = '.'
