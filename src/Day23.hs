{-# LANGUAGE BangPatterns #-}

module Day23
  ( solve1
  , solve2
  ) where

import           Data.Char          (digitToInt)
import           Data.Foldable      (foldr')
import           Data.Function      ((&))
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import           Data.List          (foldl')

type Cup = Int
type Cups = IntMap Cup
data GameState = GameState !Cups !Cup

solve1 :: IO ()
solve1 = do
  -- let start = testInput 1
  let start = input 1
      GameState after100 _ = play 100 start
  putStrLn . showCups $ GameState (M.delete 1 after100) (after100 M.! 1)

solve2 :: IO ()
solve2 = do
  -- let start = testInput 2
  let start = input 2
      GameState end _ = play 10000000 start
  print . product . take 2 $ iterate (end M.!) (end M.! 1)
  -- print . product . Seq.take 2 . Seq.drop 1 $ cupsFrom 1 end

-- Inputs

testInput :: Int -> GameState
testInput 1 = toGameState $ map digitToInt "389125467"
testInput 2 = toGameState $ toMillion "389125467"

input :: Int -> GameState
input 1 = toGameState $ map digitToInt "368195742"
input 2 = toGameState $ toMillion "368195742"

toGameState :: [Cup] -> GameState
toGameState cs = GameState cups cup
  where
    cups = M.fromList $ cs `zip` (tail cs ++ [head cs])
    cup = head cs

toMillion :: String -> [Cup]
toMillion s = given ++ [maximum given + 1 .. 1000000]
  where given = map digitToInt s

-- Business logic

play :: Int -> GameState -> GameState
play n start@(GameState cups _) =
  foldl' (&) start $ replicate n (advance (M.size cups))

advance :: Int -> GameState -> GameState
advance size (GameState cups first) = GameState cups' first'
  where
    !cups' = M.union (M.fromList changes) cups
    changes =
      [(dest, firstMoved), (lastMoved, cups M.! dest), (first, first')]
    first' = cups M.! lastMoved
    firstMoved = cups M.! first
    secondMoved = cups M.! firstMoved
    lastMoved = cups M.! secondMoved
    dest = dest' first
    dest' x
      | next x `notElem` [firstMoved, secondMoved, lastMoved] = next x
      | otherwise = dest' (next x)
    next x = 1 + ((x - 2) `mod` size)

showCups :: GameState -> String
showCups (GameState cups cup) = foldr' (\c cs -> show c ++ cs) "" .
                                take (M.size cups) $ iterate (cups M.!) cup
