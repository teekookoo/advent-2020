module Day25
  ( solve1
  , solve2
  ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

solve1 :: IO ()
solve1 = do
  -- let (pkCard, pkDoor) = testInput
  let (pkCard, pkDoor) = input
      lsCard = loopSize pkCard
      encKey = loop pkDoor lsCard
  print encKey

solve2 :: IO ()
solve2 = putStrLn "No part 2"

--

input :: (Int, Int)
input = (1965712, 19072108)

testInput :: (Int, Int)
testInput = (5764801, 17807724)

--

loopSize :: Int -> Int
loopSize pk = fromJust $ elemIndex pk loopResults
  where loopResults = iterate (advance 7) 1

loop :: Int -> Int -> Int
loop subject = go 1
  where
    go x 0 = x
    go x n = go (next x) (n - 1)
    next = advance subject

advance :: Int -> Int -> Int
advance subject x = subject * x `mod` 20201227
