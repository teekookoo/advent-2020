{-# LANGUAGE BangPatterns #-}

module Day15
  ( solve1
  , solve2
  ) where

import           Data.Function      ((&))
import qualified Data.IntMap.Strict as M
import           Data.List          (foldl')

type History = M.IntMap Int
type Previous = Int
type Turn = Int
data GameState = GameState History Previous Turn

input :: [Int]
input = [6, 13, 1, 15, 2, 0]
-- input = [0,3,6]

solve1 :: IO ()
solve1 = print num
  where (GameState _ num _) = runTo 2020 . initialize $ input

solve2 :: IO ()
solve2 = print num
  where (GameState _ num _) = runTo 30000000 . initialize $ input

initialize :: [Int] -> GameState
initialize startingNumbers = GameState historyAfter previousAfter turnAfter
  where
    !historyAfter = M.fromList $ init startingNumbers `zip` [1 ..]
    !previousAfter = last startingNumbers
    !turnAfter = length startingNumbers + 1

advance :: GameState -> GameState
advance (GameState history previous turn) = GameState history' previous' turn'
  where
    !history' = M.insert previous (turn - 1) history
    !previous' = case M.lookup previous history of Nothing -> 0
                                                   Just t  -> turn - 1 - t
    !turn' = turn + 1

runTo :: Turn -> GameState -> GameState
runTo target gs@(GameState _ _ turn)
  | turn > target = error "Target turn is lower than current turn"
  | otherwise = foldl' (&) gs $ replicate diff advance
  where diff = target - turn + 1
