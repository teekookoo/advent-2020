module Day05
  ( solve1
  , solve2
  ) where

import Data.Set (fromList, member)

type SeatCode = String
type Seat = (Int, Int)
type ID = Int

input :: IO [Seat]
input = map parseSeat . lines <$> readFile "src/inputs/input05"

solve1 :: IO ()
solve1 = input >>= print . maximum . map seatId

solve2 :: IO ()
solve2 = do
  seats <- input
  case candidates seats of
    [seat] -> print $ seatId seat
    _ -> print "Seat not found"

parseSeat :: SeatCode -> Seat
parseSeat code = (row, col)
  where
    row = fst $ f (0, 127) rowPart
    col = fst $ f (0, 7) colPart
    f = foldl (\(a, b) c -> if c `elem` "LF"
      then (a, b - (b - a + 1) `div` 2)
      else (a + (b - a + 1) `div` 2, b))
    (rowPart, colPart) = splitAt 7 code

seatId :: Seat -> ID
seatId (r, c) = 8*r + c

candidates :: [Seat] -> [Seat]
candidates occupied = filter f allSeats
  where
    f s = let sid = seatId s
              check = (`member` sids)
          in not (check sid) && check (sid - 1) && check (sid + 1)
    sids = fromList $ map seatId occupied
    allSeats = [(r, c) | r <- [0..127], c <- [0..7]]
